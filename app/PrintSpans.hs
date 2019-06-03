{-# LANGUAGE OverloadedStrings #-}

-- | A simple tracing example which outputs traces to stdout.
--
-- It also showcases how to trace calls across threads.
module Main where

import Control.Concurrent
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Monitor.Tracing
import UnliftIO (MonadUnliftIO, withRunInIO)

stdoutPublisher :: Publisher
stdoutPublisher (Just (nextSpan, _)) = print nextSpan
stdoutPublisher _ = pure ()

doSomething :: (MonadTrace m, MonadUnliftIO m) => m ()
doSomething = childSpan "outer" { builderTags = Map.singleton "hi" "hey" } $ do
  ctx <- currentContext
  annotateSpan "log2" $ logValue ("abc" :: Text)
  annotateSpan "tag2" $ tagTextValue "def"
  liftIO $ print ctx
  liftIO $ threadDelay 100000
  childSpan "A" $ liftIO $ threadDelay 10000
  forkChildSpan "nested" $ do
    liftIO . print =<< currentContext
    liftIO $ threadDelay 20000
    annotateSpan "log1" $ logValue ("abc" :: Text)
    annotateSpan "tag1" $ tagTextValue "def"
  childSpan "B" $ liftIO $ threadDelay 30000

simple :: MonadTrace m => m ()
simple = do
  annotateSpan "TEST_LOG0" $ logValue ("foo" :: Text)
  annotateSpan "TEST_TAG0" $ tagDoubleValue 123
  childSpan "nested1" $ annotateSpan "TEST_TAG1" $ tagInt64Value 12
  childSpan "nested2" $ do
    annotateSpan "TEST_TAG2" $ tagDoubleValue 12.5
    childSpan "nestednsted" $ annotateSpan "TEST_TAG3" $ tagInt64Value (-5)

main :: IO ()
main = do
  tracer <- startTracer stdoutPublisher
  trace simple tracer "root"
  trace doSomething tracer "root" { builderBaggages = Map.singleton "foo" "bar" }
  stopTracer tracer
