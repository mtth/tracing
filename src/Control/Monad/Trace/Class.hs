{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'MonadTrace' class
module Control.Monad.Trace.Class
  (
    Builder(..), builder
  , MonadTrace(..)
  , forkChildSpan
  -- $writingMetadata
  , Annotation, tagDoubleValue, tagInt64Value, tagTextValue, logValue, logValueAt
  ) where

import Monitor.Tracing.Internal

import Control.Concurrent (forkIO, ThreadId)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Identity (Identity(..))
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.RWS.Strict as RWS.Strict
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State.Strict
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified Control.Monad.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Writer.Strict as Writer.Strict
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import UnliftIO (MonadUnliftIO, withRunInIO)

class Monad m => MonadTrace m where

  -- | Starts a new span, wrapping the input action.
  childSpan :: Builder -> m a -> m a

  -- | Extracts the current span's context.
  currentContext :: m Context
  default currentContext :: (MonadTrace n, MonadTrans t, m ~ t n) => m Context
  currentContext = lift currentContext

  -- | Adds information to the current span.
  annotateSpan :: Key -> Annotation -> m ()
  default annotateSpan :: (MonadTrace n, MonadTrans t, m ~ t n) => Key -> Annotation -> m ()
  annotateSpan key = lift . annotateSpan key

instance (Monad m, MonadTrace m) => MonadTrace (ExceptT e m) where
  childSpan name (ExceptT actn) = ExceptT $ childSpan name actn

instance (Monad m, MonadTrace m) => MonadTrace (ReaderT r m) where
  childSpan name (ReaderT actn) = ReaderT $ \r -> childSpan name (actn r)

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (RWS.Lazy.RWST r w s m) where
  childSpan name (RWS.Lazy.RWST actn) = RWS.Lazy.RWST $ \r s -> childSpan name (actn r s)

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (RWS.Strict.RWST r w s m) where
  childSpan name (RWS.Strict.RWST actn) = RWS.Strict.RWST $ \r s -> childSpan name (actn r s)

instance (Monad m, MonadTrace m) => MonadTrace (State.Lazy.StateT s m) where
  childSpan name (State.Lazy.StateT actn) = State.Lazy.StateT $ \s -> childSpan name (actn s)

instance (Monad m, MonadTrace m) => MonadTrace (State.Strict.StateT s m) where
  childSpan name (State.Strict.StateT actn) = State.Strict.StateT $ \s -> childSpan name (actn s)

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (Writer.Lazy.WriterT w m) where
  childSpan name (Writer.Lazy.WriterT actn) = Writer.Lazy.WriterT $ childSpan name actn

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (Writer.Strict.WriterT w m) where
  childSpan name (Writer.Strict.WriterT actn) = Writer.Strict.WriterT $ childSpan name actn

zeroContext :: Context
zeroContext = Context (TraceID $ zeros 16) (SpanID $ zeros 8) Map.empty where
  zeros len = BS.pack $ replicate len 0

instance MonadTrace Identity where
  currentContext = pure zeroContext
  childSpan _ = id
  annotateSpan _ _ = pure ()

-- | Starts a new span inside a new thread, returning the newly created thread's ID.
--
-- This convenience method around 'forkIO' and 'withRunInIO' is provided since getting insights into
-- concurrent calls is one of the main benefits of tracing.
forkChildSpan :: (MonadTrace m, MonadUnliftIO m) => Builder -> m () -> m ThreadId
forkChildSpan bldr actn = withRunInIO $ \run -> forkIO $ run $ childSpan bldr actn

-- $writingMetadata
-- ...

-- | Generates a tag value from a double.
tagDoubleValue :: Double -> Annotation
tagDoubleValue = TagValue . JSON.toJSON

-- | Generates a 64-bit integer tag value from any integer.
tagInt64Value :: Integral a => a -> Annotation
tagInt64Value = TagValue . (JSON.toJSON @Int64) . fromIntegral

-- | Generates a Unicode text tag value.
tagTextValue :: Text -> Annotation
tagTextValue = TagValue . JSON.toJSON

-- | Generates a log value with the time of writing as timestamp. Note that the value may be written
-- later than it is created. For more control on the timestamp, use 'logValueAt'.
logValue :: JSON.ToJSON a => a -> Annotation
logValue v = LogValue (JSON.toJSON v) Nothing

-- | Generates a log value with a custom time.
logValueAt :: JSON.ToJSON a => POSIXTime -> a -> Annotation
logValueAt t v = LogValue (JSON.toJSON v) (Just t)
