{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Trace.Internal
  ( TraceID(..), randomTraceID
  , SpanID(..), randomSpanID
  , Context(..)
  , Name
  , Span(..), Interval(..), Tags, Logs
  , Reference(..)
  , Key, Value(..)
  ) where

import Control.Monad (replicateM)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Base16 as Base16
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Random (randomIO)

-- | The name of a span.
type Name = Text

-- | The type of annotations' keys.
--
-- Keys starting with double underscores are reserved and should not be used.
type Key = Text

-- | A 128-bit trace identifier.
newtype TraceID = TraceID ByteString deriving (Eq, Ord, Show)

instance JSON.FromJSON TraceID where
  parseJSON = JSON.withText "TraceID" $ \t -> case hexDecode t of
    Just bs | BS.length bs == 16 -> pure $ TraceID bs
    _ -> fail "invalid hex-encoded trace ID"

instance JSON.ToJSON TraceID where
  toJSON (TraceID bs) = JSON.toJSON $ hexEncode bs

-- | Generates a random trace ID.
randomTraceID :: IO TraceID
randomTraceID = TraceID <$> randomID 16

-- | A 64-bit span identifier.
newtype SpanID = SpanID ByteString deriving (Eq, Ord, Show)

instance JSON.FromJSON SpanID where
  parseJSON = JSON.withText "SpanID" $ \t -> case hexDecode t of
    Just bs | BS.length bs == 8 -> pure $ SpanID bs
    _ -> fail "invalid hex-encoded span ID"

instance JSON.ToJSON SpanID where
  toJSON (SpanID bs) = JSON.toJSON $ hexEncode bs

-- | Generates a random span ID.
randomSpanID :: IO SpanID
randomSpanID = SpanID <$> randomID 8

-- | A fully qualified span identifier, containing both the ID of the trace the span belongs to and
-- the span's ID. Span contexts can be exported (resp. imported) via their 'JSON.toJSON' (resp.
-- 'JSON.fromJSON') instance.
data Context = Context
  { contextTraceID :: !TraceID
  , contextSpanID :: !SpanID
  , contextBaggages :: !(Map Key ByteString)
  } deriving (Eq, Ord, Show)

-- | A relationship between spans.
--
-- There are currently two types of references, both of which model direct causal relationships
-- between a child and a parent. More background on references is available in the opentracing
-- specification: https://github.com/opentracing/specification/blob/master/specification.md.
data Reference
  = ChildOf !SpanID
  -- ^ 'ChildOf' references imply that the parent span depends on the child span in some capacity.
  -- Note that this reference type is only valid within a single trace.
  | FollowsFrom !Context
  -- ^ If the parent does not depend on the child, we use a 'FollowsFrom' reference.
  deriving (Eq, Ord, Show)

data Value
  = TagValue !JSON.Value
  | LogValue !JSON.Value !(Maybe POSIXTime)

data Span = Span
  { spanName :: !Name
  , spanContext :: !Context
  , spanReferences :: !(Set Reference)
  }

data Interval = Interval
  { intervalStart :: !POSIXTime
  , intervalDuration :: !NominalDiffTime
  }

type Tags = Map Key JSON.Value

type Logs = [(POSIXTime, Key, JSON.Value)]

randomID :: Int -> IO ByteString
randomID len = BS.pack <$> replicateM len randomIO

hexDecode :: Text-> Maybe ByteString
hexDecode t = case Base16.decode $ BS.Char8.pack $ T.unpack t of
  (bs, trail) | BS.null trail -> Just bs
  _ -> Nothing

hexEncode :: ByteString -> Text
hexEncode = T.pack . BS.Char8.unpack . Base16.encode
