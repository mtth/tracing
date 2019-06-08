{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A simple tracing example which outputs traces to stdout.
--
-- It also showcases how to trace calls across threads.
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as Zipkin
import qualified Net.IPv4 as IPv4

example1 :: MonadTrace m => m ()
example1 = Zipkin.rootSpan Zipkin.Accept "example1" $ do
  Zipkin.annotate "TEST_LOG0"
  Zipkin.localSpan "nested1" $ Zipkin.tag "TEST_TAG1" "12"
  Zipkin.localSpan "nested2" $ do
    Zipkin.tag "TEST_TAG2" "a.2"
    Zipkin.localSpan "nestednsted" $ Zipkin.tag "TEST_TAG3" ""

example2 :: (MonadTrace m, MonadUnliftIO m) => m ()
example2 = Zipkin.rootSpan Zipkin.Accept "something" $ do
  Zipkin.annotate "log2"
  Zipkin.tag "tag2" "def"
  liftIO $ threadDelay 100000
  Zipkin.localSpan "A" $ do
    void $ tracedForkIO $ Zipkin.localSpan "aa" $ do
      liftIO $ threadDelay 20000
      Zipkin.annotate "log1"
      Zipkin.tag "tag1" "def"
    liftIO $ threadDelay 10000
  Zipkin.localSpan "B" $ liftIO $ threadDelay 30000

main :: IO ()
main = do
  zipkin <- Zipkin.new $ Zipkin.defaultSettings
    { Zipkin.settingsEndpoint = Just $ Zipkin.defaultEndpoint
      { Zipkin.endpointService = Just "print-spans"
      , Zipkin.endpointPort = Just 1234
      , Zipkin.endpointIPv4 = Just IPv4.localhost
      }
    , Zipkin.settingsHost = "localhost"
    }
  Zipkin.run zipkin example1
  Zipkin.run zipkin example2
  Zipkin.flush zipkin
