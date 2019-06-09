{-# LANGUAGE OverloadedStrings #-}

{-| A simple tracing example which publishes traces to a local Zipkin server. -}
module Main where

import Control.Monad (void)
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as ZPK
import UnliftIO (MonadUnliftIO, liftIO)
import UnliftIO.Concurrent (forkIO, threadDelay)

example :: (MonadTrace m, MonadUnliftIO m) => m ()
example = ZPK.rootSpan ZPK.Accept "something" $ do
  ZPK.tag "tag.key1" "a tag value"
  threadDelay 100000
  ZPK.localSpan "nested1" $ do
    void $ forkIO $ ZPK.localSpan "concurrent" $ do
      threadDelay 20000
      ZPK.tag "tag.key2" "another tag value"
    ZPK.annotate "launched concurrent"
    threadDelay 10000
  ZPK.annotate "nested1 ended"
  ZPK.localSpan "nested2" $ threadDelay 30000

main :: IO ()
main = ZPK.with ZPK.defaultSettings $ flip ZPK.run example
