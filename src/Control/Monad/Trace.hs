{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For the MonadReader instance.

-- | This module is useful for tracing backend implementors. If you are only interested in adding
-- tracing to an application, start at "Monitor.Tracing".
module Control.Monad.Trace (
  TraceT, runTraceT,
  Tracer(..),
  Tags, Logs, Interval(..),
  newTracer
) where

import Prelude hiding (span)

import Control.Monad.Trace.Class
import Control.Monad.Trace.Internal

import Control.Applicative ((<|>))
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTChanIO, newTVarIO, readTVar, writeTChan)
import Control.Exception (finally)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), ask, asks, local, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.Aeson as JSON
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Random (randomRIO)
import UnliftIO (MonadUnliftIO, UnliftIO(..), askUnliftIO, withRunInIO, withUnliftIO)

-- | A collection of span tags.
type Tags = Map Key JSON.Value

-- | A collection of span logs, sorted in chronological order.
type Logs = [(POSIXTime, Key, JSON.Value)]

-- | Timing information about a span.
data Interval = Interval
  { intervalStart :: !POSIXTime
  , intervalDuration :: !NominalDiffTime
  }

-- | A tracer collects spans emitted inside 'TraceT'.
data Tracer = Tracer
  { tracerChannel :: TChan (Span, Tags, Logs, Interval)
  -- ^ Channel spans get written to when they complete.
  , tracerPendingCount :: TVar Int
  -- ^ The number of spans currently in flight (started but not yet completed).
  }

-- | Creates a new 'Tracer'.
newTracer :: MonadIO m => m Tracer
newTracer = liftIO $ Tracer <$> newTChanIO <*> newTVarIO 0

data Scope = Scope
  { scopeTracer :: !Tracer
  , scopeSpan :: !(Maybe Span)
  , scopeTags :: !(Maybe (TVar Tags))
  , scopeLogs :: !(Maybe (TVar Logs))
  }

-- | Asynchronous trace collection monad.
newtype TraceT m a = TraceT { traceTReader :: ReaderT Scope m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadReader r m => MonadReader r (TraceT m) where
  ask = lift ask
  local f (TraceT (ReaderT g)) = TraceT $ ReaderT $ \r -> local f $ g r

instance MonadUnliftIO m => MonadTrace (TraceT m) where
  trace bldr (TraceT reader) = TraceT $ do
    parentScope <- ask
    let
      mbParentSpn = scopeSpan parentScope
      mbParentCtx = spanContext <$> mbParentSpn
      mbTraceID = contextTraceID <$> mbParentCtx
      mbSampling = builderSampling bldr
      isDebug = fromMaybe False $ ((== Debug) <$> mbSampling) <|> (spanIsDebug <$> mbParentSpn)
    isSampled <- case mbSampling of
      Just Debug -> pure True
      Just Always -> pure True
      Just Never -> pure False
      Just (WithProbability r) -> do
        r' <- liftIO $ randomRIO (0, 1)
        pure $ r' < r
      Nothing -> pure $ maybe False spanIsSampled mbParentSpn
    spanID <- maybe (liftIO randomSpanID) pure $ builderSpanID bldr
    traceID <- maybe (liftIO randomTraceID) pure $ builderTraceID bldr <|> mbTraceID
    tagsTV <- liftIO $ newTVarIO $ builderTags bldr
    logsTV <- liftIO $ newTVarIO []
    let
      baggages = fromMaybe Map.empty $ contextBaggages <$> mbParentCtx
      ctx = Context traceID spanID (builderBaggages bldr `Map.union` baggages)
      spn = Span (builderName bldr) ctx (builderReferences bldr) isSampled isDebug
      tracer = scopeTracer parentScope
      childScope = Scope tracer (Just spn) (Just tagsTV) (Just logsTV)
    withRunInIO $ \run -> do
      start <- getPOSIXTime
      atomically $ modifyTVar' (tracerPendingCount tracer) (+1)
      run (local (const childScope) reader) `finally` do
        end <- getPOSIXTime
        atomically $ do
          modifyTVar' (tracerPendingCount tracer) (\n -> n - 1)
          tags <- readTVar tagsTV
          logs <- sortOn (\(t, k, _) -> (t, k)) <$> readTVar logsTV
          writeTChan (tracerChannel tracer) (spn, tags, logs, Interval start (end - start))

  activeSpan = TraceT $ asks scopeSpan

  addSpanEntry key (TagValue val) = TraceT $ asks scopeTags >>= \case
    Nothing -> pure ()
    Just tv -> liftIO $ atomically $ modifyTVar' tv $ Map.insert key val
  addSpanEntry key (LogValue val maybeTime)  = TraceT $ asks scopeLogs >>= \case
    Nothing -> pure ()
    Just tv -> do
      time <- case maybeTime of
        Nothing -> liftIO getPOSIXTime
        Just time' -> pure time'
      liftIO $ atomically $ modifyTVar' tv ((time, key, val) :)

instance MonadUnliftIO m => MonadUnliftIO (TraceT m) where
  askUnliftIO = TraceT $ withUnliftIO $ \u -> pure (UnliftIO (unliftIO u . traceTReader ))

-- | Trace an action.
runTraceT :: TraceT m a -> Tracer -> m a
runTraceT (TraceT reader) tracer = runReaderT reader (Scope tracer Nothing Nothing Nothing)
