#!/usr/bin/env stack
-- stack --install-ghc runghc

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import GHC.Generics (Generic)
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as ZPK
import Yesod

data Person = Person
  { personName :: Text
  , personAge  :: Int
  } deriving Generic

instance ToJSON Person

data App = App
  { appZipkin :: Zipkin
  }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

-- Adds tracing to any handler action.
serverSpan :: Handler a -> Handler a
serverSpan actn = lookupHeader "b3" >>= \case
  Nothing -> actn
  Just bs -> case ZPK.b3FromHeaderValue bs of
    Nothing -> fail "bad b3 header"
    Just b3 -> appZipkin <$> getYesod >>= ZPK.run (ZPK.serverSpan b3 $ lift actn)

getHomeR :: Handler Value
getHomeR = serverSpan $ returnJson $ Person "James" 25

main :: IO ()
main = do
  zipkin <- ZPK.new ZPK.defaultSettings
    { ZPK.settingsPublishPeriod = Just 10 -- Auto-publish spans every 10 seconds.
    , ZPK.settingsEndpoint = Just "api" }
  warp 3000 (App zipkin)
