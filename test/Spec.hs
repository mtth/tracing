{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Trace
import Control.Monad.Trace.Class
import Monitor.Tracing
import Monitor.Tracing.Local (collectSpanSamples)
import qualified Monitor.Tracing.Zipkin as ZPK

import Control.Monad (void)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, runReader, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.Lifted
import Control.Concurrent.STM.Lifted

collectSpans :: (MonadIO m, MonadBaseControl IO m) => TraceT m () -> m [Span]
collectSpans actn = fmap sampleSpan . snd <$> collectSpanSamples actn

main :: IO ()
main = hspec $ do
  describe "MonadTrace" $ do
    it "should be instantiable with identity-based monads" $ do
      let
        actn :: (MonadReader Int m , MonadState Int m, MonadTrace m) => m Int
        actn = trace "one" $ do
          s <- get
          r <- trace "two" ask
          pure $ s + r
        v = runReader (evalStateT actn 1) 2
      v `shouldBe` 3
  describe "trace" $ do
    it "should not create spans when no traces are started" $ do
      spans <- collectSpans $ pure ()
      fmap spanName spans `shouldBe` []
    it "should collect a single span when no children are created" $ do
      spans <- collectSpans (trace "t" { builderSamplingPolicy = Just alwaysSampled } $ pure ())
      fmap spanName spans `shouldBe` ["t"]
    it "should be able to stack on top of a ReaderT" $ do
      let
        actn = trace "t" { builderSamplingPolicy = Just alwaysSampled } $ do
          name <- ask
          trace (builder name) $ pure ()
      spans <- runReaderT (collectSpans @(ReaderT Text IO) actn) "foo"
      fmap spanName spans `shouldBe` ["foo", "t"]
  describe "Zipkin" $ do
    it "should round-trip a B3 using a single header" $ do
      let
        bs = "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1-05e3ac9a4f6e3b90"
        mbBs = ZPK.b3ToHeaderValue <$> ZPK.b3FromHeaderValue bs
      mbBs `shouldBe` Just bs
    it "should have equivalent B3 header representations" $ do
      let
        bs = "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1-05e3ac9a4f6e3b90"
        hdrs = Map.fromList
          [ ("X-B3-TraceId", "80f198ee56343ba864fe8b2a57d3eff7")
          , ("X-B3-SpanId", "e457b5a2e4d86bd1")
          , ("X-B3-ParentSpanId", "05e3ac9a4f6e3b90")
          , ("X-B3-Sampled", "1") ]
        Just b3 = ZPK.b3FromHeaderValue bs
        Just b3' = ZPK.b3FromHeaders hdrs
      b3 `shouldBe` b3'
  describe "collectSpanSamples" $ do
    it "should collect spans which are still pending after the action returns" $ do
      spans <- collectSpans $ rootSpan alwaysSampled "sleep-parent" $ do
        tmv <- newEmptyTMVarIO
        void $ fork $ childSpan "sleep-child" $ atomically (putTMVar tmv ()) >> threadDelay 20000
        void $ atomically $ readTMVar tmv
      fmap spanName spans `shouldMatchList` ["sleep-parent", "sleep-child"]
