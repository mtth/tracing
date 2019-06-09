{-| Non-intrusive distributed tracing

Let's assume for example we are interested in tracing the two following functions:

> listTaskIDs' :: MonadIO m => m [Int] -- Returns a list of all task IDs.
> fetchTasks' :: MonadIO m => [Int] -> m [Task] -- Resolves IDs into tasks.

We can do so simply by wrapping them inside a 'childSpan' call and adding a 'MonadTrace' constraint:

> import Monitor.Tracing
>
> listTaskIDs :: (MonadIO m, MonadTrace m) => m [Int]
> listTaskIDs = childSpan "list-task-ids" listTaskIDs'
>
> fetchTasks :: (MonadIO m, MonadTrace m) => [Int] -> m [Task]
> fetchTasks = childSpan "fetch-tasks" . fetchTasks'

Spans will now automatically get generated any time these actions are run! Each span will be
associated with various useful pieces of metadata, including lineage. For example, if we wrap the
two above functions in a 'rootSpan', the spans will correctly be nested:

> printTasks :: (MonadIO m, MonadTrace m) => m ()
> printTasks = rootSpan alwaysSampled "list-tasks" $ listTaskIDs >>= fetchTasks >>= print

Spans can then be published to various backends. For example, to run the above action and publish
its spans using Zipkin:

> import qualified Monitor.Tracing.Zipkin as ZPK
>
> main :: IO ()
> main = ZPK.with ZPK.defaultSettings $ ZPK.run printTasks

-}
module Monitor.Tracing (
  -- * Overview
  MonadTrace,
  -- * Generic span creation
  Sampling, alwaysSampled, neverSampled, sampledEvery, sampledWhen, debugEnabled,
  rootSpan, rootSpanWith, childSpan, childSpanWith,
  -- * Backends
  Zipkin
) where

import Control.Monad.Trace.Class
import Monitor.Tracing.Zipkin (Zipkin)
