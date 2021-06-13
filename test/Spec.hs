{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Trace
import Control.Monad.Trace.Class
import Monitor.Tracing
import Monitor.Tracing.Local (collectSpanSamples)
import qualified Monitor.Tracing.Zipkin as ZPK

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, runReader, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.STM

collectSpans :: MonadUnliftIO m => TraceT m () -> m [Span]
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

    it "should be runnable in IO without a tracer" $ do
      let
        actn :: (MonadIO m, MonadTrace m) => m Int
        actn = trace "one" $ do
          r <- liftIO $ newIORef 1
          trace "two" $ liftIO (readIORef r)
      v <- runTraceT' actn Nothing
      v `shouldBe` 1

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

    it "consumerSpan should use B3 as parent reference" $ do
      let
        bs = "80f198ee56343ba864fe8b2a57d3eff7-e457b5a2e4d86bd1-1-05e3ac9a4f6e3b90"
        Just b3 = ZPK.b3FromHeaderValue bs
      [consumerSpan] <- collectSpans $ ZPK.consumerSpanWith id b3 $ pure ()
      contextTraceID (spanContext consumerSpan) `shouldBe` ZPK.b3TraceID b3            -- same traceId
      contextSpanID (spanContext consumerSpan) `shouldNotBe` ZPK.b3SpanID b3           -- different spanId
      spanReferences consumerSpan `shouldBe` Set.singleton (ChildOf $ ZPK.b3SpanID b3) -- b3 spanId is parent

  describe "collectSpanSamples" $ do
    it "should collect spans which are still pending after the action returns" $ do
      spans <- collectSpans $ rootSpan alwaysSampled "sleep-parent" $ do
        tmv <- newEmptyTMVarIO
        void $ forkIO $ childSpan "sleep-child" $ atomically (putTMVar tmv ()) >> threadDelay 20000
        void $ atomically $ readTMVar tmv
      fmap spanName spans `shouldMatchList` ["sleep-parent", "sleep-child"]
