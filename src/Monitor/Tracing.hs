-- | Distributed tracing
--
-- A lightweight, testable, and opentracing-compliant tracing library.
--
--
--
module Monitor.Tracing
  (
  -- * Overview
  -- $overview
    MonadTrace
  , tracedForkIO
  -- * Convenience exports
  , MonadIO, liftIO
  , MonadUnliftIO, withRunInIO
  ) where

import Control.Monad.Trace.Class

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad.IO.Class (MonadIO, liftIO)
import UnliftIO (MonadUnliftIO, withRunInIO)

-- $overview
--
-- Assume we are interested in tracing the two following functions:
--
-- @
--  listTaskIDs :: MonadIO m => m [Int] -- Returns a list of all task IDs.
--  fetchTasks :: MonadIO m => [Int] -> m [Task] -- Resolves IDs into tasks.
-- @
--
-- We can do so simply by wrapping them inside a 'childSpan' call and adding a 'MonadTrace'
-- constraint:
--
-- @
--  listTaskIDs' :: (MonadIO m, MonadTrace m) => m [Int]
--  listTaskIDs' = childSpan "list-task-ids" listTaskIDs
--
--  fetchTasks' :: (MonadIO m, MonadTrace m) => [Int] -> m [Task]
--  fetchTasks' = childSpan "fetch-tasks" . fetchTasks
-- @
--
-- Spans will now automatically get generated each time these actions are run!
--
-- Each 'Span' includes various useful pieces of metadata, including lineage. For example, if we
-- wrap the two above functions in another, the spans will correctly be nested:
--
-- @
--  listTasks' :: (MonadIO m, MonadTrace m) => m [Task]
--  listTasks' = childSpan "list-tasks" $ listTaskIDs >>= fetchTasks
-- @
--
-- To extract the generated spans:
--
-- @
--  main :: IO ()
--  main = do
--    tracer <- startTracer $ zipkinPublisher "http://localhost:1234"
--    tasks <- trace tracer "" listTasks'
--    stopTracer tracer
--    print tasks
-- @

-- | Starts a new span inside a new thread, returning the newly created thread's ID.
--
-- This convenience method around 'forkIO' and 'withRunInIO' is provided since getting insights into
-- concurrent calls is one of the main benefits of tracing.
tracedForkIO :: (MonadTrace m, MonadUnliftIO m) => m () -> m ThreadId
tracedForkIO actn = withRunInIO $ \run -> forkIO $ run actn
