{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as ZPK
import qualified Network.HTTP.Simple as HTTP
import UnliftIO
import UnliftIO.Concurrent

run :: (MonadIO m, MonadTrace m) => m ()
run = rootSpan alwaysSampled "run" $ do
  threadDelay 2000
  ZPK.annotate "sending"
  ZPK.clientSpan "call" $ \(Just b3) -> do
    let
      req = "http://localhost:3000"
        & HTTP.addRequestHeader "b3" (ZPK.b3ToHeaderValue b3)
    liftIO $ HTTP.httpBS req >>= print

main :: IO ()
main = do
  let settings = ZPK.defaultSettings { ZPK.settingsEndpoint = Just "cli" }
  ZPK.with settings $ ZPK.run run
