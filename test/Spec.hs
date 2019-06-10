{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Trace
import Control.Monad.Trace.Class
import Monitor.Tracing
import qualified Monitor.Tracing.Zipkin as ZPK

import Control.Concurrent
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, runReader, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck
import UnliftIO (MonadUnliftIO)

collectSpans :: MonadUnliftIO m => TraceT m () -> m [Span]
collectSpans actn = do
  tracer <- newTracer
  runTraceT actn tracer
  ref <- liftIO $ newIORef []
  liftIO $ fix $ \loop -> atomically (tryReadTChan $ tracerChannel tracer) >>= \case
    Nothing -> pure ()
    Just spl -> modifyIORef ref (sampleSpan spl:) >> loop
  reverse <$> liftIO (readIORef ref)

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
      spans <- collectSpans @IO (pure ())
      fmap spanName spans `shouldBe` []
    it "should collect a single span when no children are created" $ do
      spans <- collectSpans @IO (trace "t" { builderSamplingPolicy = Just alwaysSampled } $ pure ())
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
