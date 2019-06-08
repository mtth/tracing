{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For the MonadReader instance.

-- | The 'TraceT' class.
module Control.Monad.Trace
  ( Tracer, newTracer, tracerChannel, tracerPendingCount
  , TraceT, runTraceT
  ) where

import Prelude hiding (span)

import Control.Monad.Trace.Class
import Monitor.Tracing.Internal

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TChan, TMVar, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (Exception, finally, throw)
import Control.Monad (void, when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), ask, asks, local, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import UnliftIO (MonadUnliftIO, UnliftIO(..), askUnliftIO, withRunInIO, withUnliftIO)

data Tracer = Tracer
  { _tracerChannel :: TChan Span
  , _tracerPending :: TVar Int
  }

newTracer :: MonadIO m => m Tracer
newTracer = liftIO $ Tracer <$> STM.newTChanIO <*> STM.newTVarIO 0

tracerChannel :: Tracer -> TChan Span
tracerChannel = _tracerChannel

tracerPendingCount :: Tracer -> TVar Int
tracerPendingCount = _tracerPending

data Scope = Scope
  { _scopeTracer :: Tracer
  , _scopeActiveSpan :: Maybe (TVar ActiveSpan)
  }

-- Asynchronous trace collection monad.
--
-- If the tracer is closed before a 'trace' call, 'trace' will throw 'TracerClosed'.
newtype TraceT m a = TraceT { traceTReader :: ReaderT Scope m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadReader r m => MonadReader r (TraceT m) where
  ask = lift ask
  local f (TraceT (ReaderT g)) = TraceT $ ReaderT $ \r -> local f $ g r

mapActiveSpan :: MonadIO m => a -> (TVar ActiveSpan -> STM a) -> ReaderT Scope m a
mapActiveSpan v f = asks _scopeActiveSpan >>= \case
  Nothing -> pure v
  Just tv -> liftIO $ STM.atomically $ f tv

readActiveSpan :: MonadIO m => ReaderT Scope m (Maybe ActiveSpan)
readActiveSpan = mapActiveSpan Nothing (fmap Just . STM.readTVar)

instance MonadUnliftIO m => MonadTrace (TraceT m) where
  activeSpan = TraceT $ readActiveSpan

  trace bldr (TraceT reader) = TraceT $ do
    parentScope <- ask
    maybeOldCtx <- fmap _activeSpanContext <$> readActiveSpan
    let
      maybeTraceID = _contextTraceID <$> maybeOldCtx
      baggages = fromMaybe Map.empty $ _contextBaggages <$> maybeOldCtx
    spanID <- maybe (liftIO randomSpanID) pure $ builderSpanID bldr
    traceID <- maybe (liftIO randomTraceID) pure $ builderTraceID bldr <|> maybeTraceID
    let
      ctx = Context traceID spanID (builderBaggages bldr `Map.union` baggages)
      newSpan = ActiveSpan ctx (builderName bldr) (builderReferences bldr) (builderTags bldr) []
      tracer = _scopeTracer parentScope
    activeSpanTV <- liftIO $ STM.newTVarIO newSpan
    withRunInIO $ \run -> do
      startTime <- getPOSIXTime
      STM.atomically $ STM.modifyTVar (_tracerPending tracer) (+1)
      run (local (const $ Scope tracer (Just activeSpanTV)) reader) `finally` do
        endTime <- getPOSIXTime
        STM.atomically $ do
          activeSpan <- STM.readTVar activeSpanTV
          STM.modifyTVar (_tracerPending tracer) (\n -> n - 1)
          STM.writeTChan (_tracerChannel $ tracer) (freezeSpan activeSpan startTime endTime)

  annotate key (TagValue val) =
    let addTag spn = spn { _activeSpanTags = Map.insert key val (_activeSpanTags spn) }
    in TraceT $ mapActiveSpan () $ flip STM.modifyTVar' addTag
  annotate key (LogValue val maybeTime)  = TraceT $ do
    time <- case maybeTime of
      Nothing -> liftIO getPOSIXTime
      Just time' -> pure time'
    let addLog spn = spn { _activeSpanLogs = (time, key, val) : _activeSpanLogs spn }
    mapActiveSpan () $ flip STM.modifyTVar' addLog

instance MonadUnliftIO m => MonadUnliftIO (TraceT m) where
  askUnliftIO = TraceT $ withUnliftIO $ \u -> pure (UnliftIO (unliftIO u . traceTReader ))

-- | Trace an action.
runTraceT :: TraceT m a -> Tracer -> m a
runTraceT (TraceT reader) tracer = runReaderT reader (Scope tracer Nothing)
