{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For the MonadReader instance.

-- | The 'TraceT' class.
module Control.Monad.Trace
  ( TraceT, trace
  , Tracer, startTracer, Publisher, stopTracer
  ) where

import Prelude hiding (span)

import Control.Monad.Trace.Class
import Monitor.Tracing.Internal

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, TMVar, TVar)
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
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import UnliftIO (MonadUnliftIO, UnliftIO(..), askUnliftIO, withRunInIO, withUnliftIO)

type Publisher = Maybe (Span, Int) -> IO ()

data Tracer = Tracer
  { _tracerChan :: TChan (Maybe Span)
  , _tracerClosed :: TMVar () -- Filled when the publisher is done.
  , _tracerPendingCount :: TVar Int -- Number of elements in the channel.
  }

startTracer :: MonadIO m => Publisher -> m Tracer
startTracer accept = liftIO $ do
  allSpans <- STM.newTChanIO
  pending <- STM.newTVarIO 0
  closed <- STM.newEmptyTMVarIO
  let
    publish = fix $ \loop -> do
      let
        decr c = let c' = c - 1 in (c', c')
        stm = (,) <$> STM.readTChan allSpans <*> STM.stateTVar pending decr
      (mbSpan, count) <- STM.atomically stm
      case mbSpan of
        Just nextSpan -> accept (Just (nextSpan, count)) >> loop
        _ -> accept Nothing
  _ <- forkIO $ publish `finally` STM.atomically (STM.putTMVar closed ())
  pure $ Tracer allSpans closed pending

stopTracer :: MonadIO m => Tracer -> m ()
stopTracer tracer = liftIO $ do
  STM.atomically $ STM.writeTChan (_tracerChan tracer) Nothing
  void $ STM.atomically $ STM.readTMVar (_tracerClosed tracer)

data Scope = Scope
  { _scopeTracer :: Tracer
  , _scopeContext :: Context
  , _scopeTags :: TVar (Map Key Aeson.Value)
  , _scopeLogs :: TVar [(POSIXTime, Key, Aeson.Value)]
  }

newtype TraceT m a = TraceT { runTraceT :: ReaderT Scope m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance MonadReader r m => MonadReader r (TraceT m) where
  ask = lift ask
  local f (TraceT (ReaderT g)) = TraceT $ ReaderT $ \r -> local f $ g r

instance MonadUnliftIO m => MonadTrace (TraceT m) where
  currentContext = TraceT $ asks _scopeContext

  childSpan bldr (TraceT reader) = TraceT $ do
    parentScope <- ask
    childSpanID <- liftIO randomSpanID
    logs <- liftIO $ STM.newTVarIO []
    tags <- liftIO $ STM.newTVarIO $ builderTags bldr
    let
      name = builderName bldr
      parentCtx = _scopeContext parentScope
      baggages = builderBaggages bldr `Map.union` _contextBaggages parentCtx
      ctx = Context (_contextTraceID parentCtx) childSpanID baggages
      refs = Set.insert (ChildOf parentCtx) (builderReferences bldr)
      scope = Scope (_scopeTracer parentScope) ctx tags logs
    local (const scope) $ traceReader reader name refs

  annotateSpan key (TagValue val) = TraceT $ do
    tags <- asks _scopeTags
    liftIO $ STM.atomically $ STM.modifyTVar' tags (Map.insert key val)
  annotateSpan key (LogValue val maybeTime)  = TraceT $ do
    time <- case maybeTime of
      Nothing -> liftIO getPOSIXTime
      Just time' -> pure time'
    logs <- asks _scopeLogs
    liftIO $ STM.atomically $ STM.modifyTVar' logs ((time, key, val):)

instance MonadUnliftIO m => MonadUnliftIO (TraceT m) where
  askUnliftIO = TraceT $ withUnliftIO $ \u -> pure (UnliftIO (unliftIO u . runTraceT ))

-- | Trace an action.
--
-- If the tracer is closed...
trace :: MonadUnliftIO m => TraceT m a -> Tracer -> Builder -> m a
trace (TraceT reader) tracer bldr = do
  let
    name = builderName bldr
    refs = builderReferences bldr
    baggages = builderBaggages bldr
    parentIDs = Set.map (_contextTraceID . fromJust) $ Set.filter isJust $ Set.map parentContext refs
  traceID <- case Set.minView parentIDs of
    Just (parentID, otherIDs) | Set.null otherIDs -> pure parentID
    _ -> liftIO randomTraceID
  ctx <-  Context traceID <$> liftIO randomSpanID <*> pure baggages
  tags <- liftIO $ STM.newTVarIO $ builderTags bldr
  logs <- liftIO $ STM.newTVarIO []
  runReaderT (traceReader reader name refs) (Scope tracer ctx tags logs)

parentContext :: Reference -> Maybe Context
parentContext (ChildOf ctx) = Just ctx
parentContext _ = Nothing

data TracingException = TracerClosed deriving Show

instance Exception TracingException

traceReader :: MonadUnliftIO m => ReaderT Scope m a -> Name -> Set Reference -> ReaderT Scope m a
traceReader reader name refs = do
  scope <- ask
  withRunInIO $ \run -> do
    startTime <- getPOSIXTime
    run reader `finally` do
      endTime <- getPOSIXTime
      let tracer = _scopeTracer scope
      closed <- STM.atomically $ STM.tryReadTMVar (_tracerClosed tracer) >>= \case
        Just _ -> pure True
        Nothing -> do
          tags <- STM.readTVar $ _scopeTags scope
          logs <- reverse <$> STM.readTVar (_scopeLogs scope)
          let
            ctx = _scopeContext scope
            duration = endTime - startTime
            span = Span ctx name refs startTime duration tags logs
          STM.modifyTVar (_tracerPendingCount tracer) (+1)
          STM.writeTChan (_tracerChan $ tracer) (Just span)
          pure False
      when closed $ throw TracerClosed
