{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Monitor.Tracing

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, runReader, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, get)
import Data.IORef
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck
import UnliftIO (MonadUnliftIO)

collectSpans :: MonadUnliftIO m => TraceT m () -> Builder -> m [Span]
collectSpans actn bldr = do
  ref <- liftIO $ newIORef []
  let
    publish (Just (nextSpan, _)) = modifyIORef ref (nextSpan:)
    publish _ = pure ()
  tracer <- startTracer publish
  trace actn tracer bldr
  stopTracer tracer
  liftIO $ reverse <$> readIORef ref

main :: IO ()
main = hspec $ do
  describe "MonadTrace" $ do
    it "should be instantiable with identity-based monads" $ do
      let
        actn :: (MonadReader Int m , MonadState Int m, MonadTrace m) => m Int
        actn = childSpan "one" $ do
          s <- get
          r <- childSpan "two" ask
          pure $ s + r
        v = runReader (evalStateT actn 1) 2
      v `shouldBe` 3
  describe "trace" $ do
    it "should create a single span when no children are created" $ do
      spans <- collectSpans @IO (pure ()) "t1"
      fmap spanName spans `shouldBe` ["t1"]
    it "should create two spans when a single child is created" $ do
      spans <- flip (collectSpans @IO) "t2" $ childSpan "c2" $ pure ()
      fmap spanName spans `shouldBe` ["c2", "t2"]
    it "should be able to stack on top of a ReaderT" $ do
      spans <- flip (collectSpans @IO) "t2" $ childSpan "c2" $ pure ()
      let
        actn = do
          name <- ask
          childSpan (builder name) $ pure ()
      spans <- runReaderT (collectSpans @(ReaderT Text IO) actn "t") "foo"
      fmap spanName spans `shouldBe` ["foo", "t"]
