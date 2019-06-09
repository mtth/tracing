{-# LANGUAGE OverloadedStrings #-}

{-| A simple tracing example which publishes traces to a local Zipkin server. -}
module Main where

import Control.Monad (void)
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as ZPK
import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.Concurrent (forkIO, threadDelay)

example :: (MonadTrace m, MonadUnliftIO m) => m ()
example = rootSpan alwaysSampled "something" $ do
  ZPK.tag "tag.key1" "a tag value"
  threadDelay 100000
  childSpan "nested1" $ do
    void $ forkIO $ childSpan "concurrent" $ do
      threadDelay 20000
      ZPK.tag "tag.key2" "another tag value"
    ZPK.annotate "launched concurrent"
    threadDelay 10000
  ZPK.annotate "nested1 ended"
  childSpan "nested2" $ threadDelay 30000

main :: IO ()
main = ZPK.with ZPK.defaultSettings $ ZPK.run example
