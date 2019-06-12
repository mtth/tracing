#!/usr/bin/env stack
-- stack --install-ghc runghc

{-# LANGUAGE OverloadedStrings #-}

-- | A simple tracing example which publishes traces to a local Zipkin server. Once you have a
-- working local Zikpin server, can run this script using @stack Zipkin.hs@.
module Main where

import Control.Monad (void)
import Control.Monad.Trace.Class (rootSpanWith)
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as ZPK
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO, threadDelay)

example :: (MonadTrace m, MonadUnliftIO m) => m ()
example = rootSpanWith (ZPK.addInheritedTag "id" "1234") alwaysSampled "example" $ do
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
