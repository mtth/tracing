-- | This module is where you should start if you are interested in adding tracing to an
-- application. It provides backend-agnostic utilities to generate traces. Trace publication and
-- other backend-specific features are available in the modules below @Monitor.Tracing@ (e.g.
-- "Monitor.Tracing.Zipkin"). The additional functionality exposed under @Control.Monad@ in this
-- package is useful if you wish to implement a new tracing backend.
module Monitor.Tracing (
  -- * Overview
  -- | Let's assume we are interested in tracing the two following functions:
  --
  -- > listTaskIDs' :: MonadIO m => m [Int] -- Returns a list of all task IDs.
  -- > fetchTasks' :: MonadIO m => [Int] -> m [Task] -- Resolves IDs into tasks.
  --
  -- We can do so simply by wrapping them inside 'childSpan' calls and adding a 'MonadTrace'
  -- constraint:
  --
  -- > import Monitor.Tracing
  -- >
  -- > listTaskIDs :: (MonadIO m, MonadTrace m) => m [Int]
  -- > listTaskIDs = childSpan "list-task-ids" listTaskIDs'
  -- >
  -- > fetchTasks :: (MonadIO m, MonadTrace m) => [Int] -> m [Task]
  -- > fetchTasks = childSpan "fetch-tasks" . fetchTasks'
  --
  -- Spans will now automatically get generated any time these actions are run! Each span will be
  -- associated with various useful pieces of metadata, including lineage. For example, if we wrap
  -- the two above functions in a 'rootSpan', the spans will correctly be nested:
  --
  -- > printTasks :: (MonadIO m, MonadTrace m) => m ()
  -- > printTasks = rootSpan alwaysSampled "list-tasks" $ listTaskIDs >>= fetchTasks >>= print
  --
  -- Spans can then be published to various backends. For example, to run the above action and
  -- publish its spans using Zipkin:
  --
  -- > import qualified Monitor.Tracing.Zipkin as ZPK
  -- >
  -- > main :: IO ()
  -- > main = ZPK.with ZPK.defaultSettings $ ZPK.run printTasks

  -- * Trace creation
  MonadTrace,

  -- ** Starting a new trace
  -- | By default, traces created by 'trace' are independent from each other. However, we can get a
  -- lot more value out of tracing by organizing a trace's spans. The simplest and most common
  -- approach is to build a tree of spans, with a single root span and zero or more children for
  -- each span. 'rootSpan' and 'childSpan' below set up spans such that lineage information is
  -- automatically propagated.
  rootSpan, alwaysSampled, neverSampled, sampledWhen, sampledWithProbability, debugEnabled,

  -- ** Extending a trace
  childSpan,

  -- * Backends
  -- | As a convenience, the top-level type for each backend is exported here.
  Zipkin
) where

import Control.Monad.Trace.Class
import Monitor.Tracing.Zipkin (Zipkin)
