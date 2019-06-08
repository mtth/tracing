{-# LANGUAGE OverloadedStrings #-}

module Monitor.Tracing.Internal (
  TraceID(..), randomTraceID,
  SpanID(..), randomSpanID,
  Context(..), parentSpanID,
  Name,
  ActiveSpan(..), freezeSpan,
  Span(..),
  Reference(..),
  Builder(..), builder,
  Key, Annotation(..)
) where

import Control.Monad (replicateM)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Base16 as Base16
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
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
  { _contextTraceID :: !TraceID
  , _contextSpanID :: !SpanID
  , _contextBaggages :: !(Map Key ByteString)
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

parentSpanID :: Reference -> Maybe SpanID
parentSpanID (ChildOf spanID) = Just spanID
parentSpanID _ = Nothing

data Annotation
  = TagValue !JSON.Value
  | LogValue !JSON.Value !(Maybe POSIXTime)

-- | A still-active part of a trace.
data ActiveSpan = ActiveSpan
  { _activeSpanContext :: !Context
  , _activeSpanName :: !Name
  , _activeSpanReferences :: !(Set Reference)
  , _activeSpanTags :: !(Map Key JSON.Value)
  , _activeSpanLogs :: ![(POSIXTime, Key, JSON.Value)]
  } deriving Show

freezeSpan :: ActiveSpan -> POSIXTime -> POSIXTime -> Span
freezeSpan (ActiveSpan ctx name refs tags logs) start end =
  Span ctx name refs start (end - start) tags (sortOn (\(t, k, _) -> (t, k)) logs)

-- | A piece of a trace.
data Span = Span
  { _spanContext :: !Context
  , _spanName :: !Name
  , _spanReferences :: !(Set Reference)
  , _spanStartTime :: !POSIXTime
  , _spanDuration :: !NominalDiffTime
  , _spanTags :: !(Map Key JSON.Value)
  , _spanLogs :: ![(POSIXTime, Key, JSON.Value)] -- ^ Sorted in natural order.
  } deriving Show

-- | A trace builder.
--
-- Note that 'Builder' has an 'IsString' instance, producing a span with the given string as name,
-- no additional references, tags, or baggages. This allows convenient creation of spans via the
-- @OverloadedStrings@ pragma.
data Builder = Builder
  { builderName :: !Text
  -- ^ Name of the generated span.
  , builderTraceID :: !(Maybe TraceID)
  -- ^ The trace ID of the generated span. If unset, the active span's trace ID will be used if
  -- present, otherwise a new ID will be generated.
  , builderSpanID :: !(Maybe SpanID)
  -- ^ The ID of the generated span, otherwise the ID will be auto-generated. In general you should
  -- not set this (and if you do, the trace ID should also be set).
  , builderReferences :: !(Set Reference)
  -- ^ Additional references to add.
  , builderTags :: !(Map Key JSON.Value)
  -- ^ Initial set of tags for the root span.
  , builderBaggages :: !(Map Key ByteString)
  -- ^ Additional baggages to add to the span, forwarded to all children.
  } deriving Show

-- | Returns a builder with the given input as name and all other fields empty.
builder :: Name -> Builder
builder name = Builder name Nothing Nothing Set.empty Map.empty Map.empty

instance IsString Builder where
  fromString = builder . T.pack

randomID :: Int -> IO ByteString
randomID len = BS.pack <$> replicateM len randomIO

hexDecode :: Text-> Maybe ByteString
hexDecode t = case Base16.decode $ BS.Char8.pack $ T.unpack t of
  (bs, trail) | BS.null trail -> Just bs
  _ -> Nothing

hexEncode :: ByteString -> Text
hexEncode = T.pack . BS.Char8.unpack . Base16.encode
