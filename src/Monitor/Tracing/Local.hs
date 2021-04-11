-- | This module provides convenience functionality to debug traces locally. For production use,
-- prefer alternatives, e.g. "Monitor.Tracing.Zipkin".
module Monitor.Tracing.Local (
  collectSpanSamples
) where

import Control.Concurrent.STM (atomically, readTVar, flushTQueue, readTQueue)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trace
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import UnliftIO (MonadUnliftIO)

-- | Runs a 'TraceT' action, returning any collected samples alongside its output. The samples are
-- sorted chronologically by completion time (e.g. the head is the first span to complete).
--
-- Spans which start before the action returns are guaranteed to be collected, even if they complete
-- after (in this case collection will block until their completion). More precisely,
-- 'collectSpanSamples' will return the first time there are no pending spans after the action is
-- done. For example:
--
-- > collectSpanSamples $ rootSpan alwaysSampled "parent" $ do
-- >   forkIO $ childSpan "child" $ threadDelay 2000000 -- Asynchronous 2 second child span.
-- >   threadDelay 1000000 -- Returns after one second, but the child span will still be sampled.
collectSpanSamples :: MonadUnliftIO m => TraceT m a -> m (a, [Sample])
collectSpanSamples actn = do
  tracer <- newTracer
  rv <- runTraceT actn tracer
  liftIO $ do
    ref <- newIORef []
    let
      addSample spl = modifyIORef' ref (spl:)
      samplesTQ = spanSamples tracer
      pendingTV = pendingSpanCount tracer
    fix $ \loop -> do
      (samples, pending) <- atomically $ (,) <$> flushTQueue samplesTQ <*> readTVar pendingTV
      for_ samples addSample
      when (pending > 0) $ (atomically $ readTQueue samplesTQ) >>= addSample >> loop
    spls <- reverse <$> readIORef ref
    pure (rv, spls)
