#!/usr/bin/env stack
-- stack --install-ghc runghc

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function ((&))
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as ZPK
import qualified Network.HTTP.Simple as HTTP

run :: (MonadIO m, MonadTrace m) => m ()
run = ZPK.clientSpan "run" $ \(Just b3) -> do
  let req = "http://localhost:3000" & HTTP.addRequestHeader "b3" (ZPK.b3ToHeaderValue b3)
  liftIO $ HTTP.httpBS req >>= print

main :: IO ()
main = do
  let settings = ZPK.defaultSettings { ZPK.settingsEndpoint = Just "cli" }
  ZPK.with settings $ ZPK.run $ rootSpan alwaysSampled "main" run
