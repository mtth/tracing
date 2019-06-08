{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'MonadTrace' class
module Control.Monad.Trace.Class
  (
    Builder(..), builder
  , MonadTrace(..)
  , ActiveSpan(..)
  , tracedFork
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
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Int (Int64)
import Data.Text (Text)
import UnliftIO (MonadUnliftIO, withRunInIO)

class Monad m => MonadTrace m where

  -- | Starts a new child span, wrapping the input action.
  trace :: Builder -> m a -> m a

  -- | Extracts the currently active span. Note that this span may evolve further.
  activeSpan :: m (Maybe ActiveSpan)
  default activeSpan :: (MonadTrace n, MonadTrans t, m ~ t n) => m (Maybe ActiveSpan)
  activeSpan = lift activeSpan

  -- | Adds information to the active span.
  annotate :: Key -> Annotation -> m ()
  default annotate :: (MonadTrace n, MonadTrans t, m ~ t n) => Key -> Annotation -> m ()
  annotate key = lift . annotate key

instance (Monad m, MonadTrace m) => MonadTrace (ExceptT e m) where
  trace name (ExceptT actn) = ExceptT $ trace name actn

instance (Monad m, MonadTrace m) => MonadTrace (ReaderT r m) where
  trace name (ReaderT actn) = ReaderT $ \r -> trace name (actn r)

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (RWS.Lazy.RWST r w s m) where
  trace name (RWS.Lazy.RWST actn) = RWS.Lazy.RWST $ \r s -> trace name (actn r s)

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (RWS.Strict.RWST r w s m) where
  trace name (RWS.Strict.RWST actn) = RWS.Strict.RWST $ \r s -> trace name (actn r s)

instance (Monad m, MonadTrace m) => MonadTrace (State.Lazy.StateT s m) where
  trace name (State.Lazy.StateT actn) = State.Lazy.StateT $ \s -> trace name (actn s)

instance (Monad m, MonadTrace m) => MonadTrace (State.Strict.StateT s m) where
  trace name (State.Strict.StateT actn) = State.Strict.StateT $ \s -> trace name (actn s)

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (Writer.Lazy.WriterT w m) where
  trace name (Writer.Lazy.WriterT actn) = Writer.Lazy.WriterT $ trace name actn

instance (Monad m, MonadTrace m, Monoid w) => MonadTrace (Writer.Strict.WriterT w m) where
  trace name (Writer.Strict.WriterT actn) = Writer.Strict.WriterT $ trace name actn

instance MonadTrace Identity where
  trace _ = id
  activeSpan = pure Nothing
  annotate _ _ = pure ()

-- | Starts a new span inside a new thread, returning the newly created thread's ID.
--
-- This convenience method around 'forkIO' and 'withRunInIO' is provided since getting insights into
-- concurrent calls is one of the main benefits of tracing.
tracedFork :: (MonadTrace m, MonadUnliftIO m) => m () -> m ThreadId
tracedFork actn = withRunInIO $ \run -> forkIO $ run actn

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
