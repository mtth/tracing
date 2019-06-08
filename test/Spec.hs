{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Trace
import Control.Monad.Trace.Class
import Monitor.Tracing

import Control.Concurrent
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, runReader, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get)
import Data.IORef
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
    Just spn -> modifyIORef ref (spn:) >> loop
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
    it "should create a no spans when no traces are created" $ do
      spans <- collectSpans @IO (pure ())
      fmap spanName spans `shouldBe` []
    it "should create a single span when no children are created" $ do
      spans <- collectSpans @IO (trace "t0" $ pure ())
      fmap spanName spans `shouldBe` ["t0"]
    it "should be able to stack on top of a ReaderT" $ do
      spans <- (collectSpans @IO) $ trace "c2" $ pure ()
      let
        actn = trace "t" $ do
          name <- ask
          trace (builder name) $ pure ()
      spans <- runReaderT (collectSpans @(ReaderT Text IO) actn) "foo"
      fmap spanName spans `shouldBe` ["foo", "t"]
