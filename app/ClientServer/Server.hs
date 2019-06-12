{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Maybe (maybe)
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

activeB3 :: Handler (Maybe ZPK.B3)
activeB3 = maybe Nothing ZPK.b3FromHeaderValue <$> lookupHeader "b3" where

getHomeR :: Handler Value
getHomeR = activeB3 >>= \case
    Nothing -> returnJson $ Person "Not found" 25
    Just b3 -> do
      zipkin <- appZipkin <$> getYesod
      let actn = ZPK.serverSpan b3 $ returnJson $ Person "James" 25
      ZPK.run actn zipkin

main :: IO ()
main = do
  zipkin <- ZPK.new ZPK.defaultSettings
    { ZPK.settingsPublishPeriod = Just 10
    , ZPK.settingsEndpoint = Just "api" { ZPK.endpointPort = Just 3000 } }
  warp 3000 (App zipkin)
