{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This module implements a <https://zipkin.apache.org/ Zipkin>-powered trace publisher. You will
-- almost certainly want to import it qualified.
--
-- Zipkin does not support all OpenTracing functionality. To guarantee that everything works as
-- expected, you should only use the functions defined in this module or exported by
-- "Monitor.Tracing".
module Monitor.Tracing.Zipkin (
  -- * Configuration
  -- ** General settings
  Settings(..), defaultSettings,
  -- ** Endpoint
  Endpoint(..), defaultEndpoint,

  -- * Publishing traces
  Zipkin,
  new, run, publish, with,

  -- * Cross-process spans
  -- ** Communication
  B3(..), b3ToHeaders, b3FromHeaders, b3ToHeaderValue, b3FromHeaderValue, b3FromSpan,
  -- ** Span generation
  clientSpan, clientSpanWith, serverSpan, serverSpanWith, producerSpanWith, consumerSpanWith,

  -- * Custom metadata
  -- ** Tags
  tag, addTag, addInheritedTag, addConsumerKind, addProducerKind,
  -- ** Annotations
  -- | Annotations are similar to tags, but timestamped.
  annotate, annotateAt,
  -- ** Endpoints
  addEndpoint
) where

import Control.Monad.Trace
import Control.Monad.Trace.Class

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad (forever, guard, void, when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI)
import Data.Time.Clock (NominalDiffTime)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, maybeToList)
import Data.Monoid (Endo(..))
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup ((<>))
#endif
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX (POSIXTime)
import Network.HTTP.Client (Manager, Request)
import qualified Network.HTTP.Client as HTTP
import Network.Socket (HostName, PortNumber)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (finally)

-- | 'Zipkin' creation settings.
data Settings = Settings
  { settingsHostname :: !(Maybe HostName)
  -- ^ The Zipkin server's hostname, defaults to @localhost@ if unset.
  , settingsPort :: !(Maybe PortNumber)
  -- ^ The port the Zipkin server is listening on, defaults to @9411@ if unset.
  , settingsEndpoint :: !(Maybe Endpoint)
  -- ^ Local endpoint included in all published spans.
  , settingsManager :: !(Maybe Manager)
  -- ^ An optional HTTP manager to use for publishing spans on the Zipkin server.
  , settingsPublishPeriod :: !(Maybe NominalDiffTime)
  -- ^ If set to a positive value, traces will be flushed in the background every such period.
  }

-- | Creates empty 'Settings'. You will typically use this (or the 'IsString' instance) as starting
-- point to only fill in the fields you care about:
--
-- > let settings = defaultSettings { settingsPort = Just 2222 }
defaultSettings :: Settings
defaultSettings = Settings Nothing Nothing Nothing Nothing Nothing

-- | Generates settings with the given string as hostname.
instance IsString Settings where
  fromString s = defaultSettings { settingsHostname = Just s }

-- | A Zipkin trace publisher.
--
-- All publisher functionality is thread-safe. In particular it is safe to 'publish' concurrently
-- with 'run', and/or 'run' multiple actions concurrently. Note also that all sampled spans are
-- retained in memory until they are published.
data Zipkin = Zipkin
  { zipkinManager :: !Manager
  , zipkinRequest :: !Request
  , zipkinTracer :: !Tracer
  , zipkinEndpoint :: !(Maybe Endpoint)
  }

flushSpans :: Maybe Endpoint -> Tracer -> Request -> Manager -> IO ()
flushSpans ept tracer req mgr = do
  ref <- newIORef []
  fix $ \loop -> atomically (tryReadTChan $ spanSamples tracer) >>= \case
    Nothing -> pure ()
    Just sample -> modifyIORef ref (ZipkinSpan ept sample:) >> loop
  spns <- readIORef ref
  when (not $ null spns) $ do
    let req' = req { HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode spns }
    void $ HTTP.httpLbs req' mgr

-- | Creates a 'Zipkin' publisher for the input 'Settings'.
new :: MonadIO m => Settings -> m Zipkin
new (Settings mbHostname mbPort mbEpt mbMgr mbPrd) = liftIO $ do
  mgr <- maybe (HTTP.newManager HTTP.defaultManagerSettings) pure mbMgr
  tracer <- newTracer
  let
    req = HTTP.defaultRequest
      { HTTP.method = "POST"
      , HTTP.host = BS.pack (fromMaybe "localhost" mbHostname)
      , HTTP.path = "/api/v2/spans"
      , HTTP.port = maybe 9411 fromIntegral mbPort
      , HTTP.requestHeaders = [("content-type", "application/json")]
      }
  void $ let prd = fromMaybe 0 mbPrd in if prd <= 0
    then pure Nothing
    else fmap Just $ forkIO $ forever $ do
      threadDelay (microSeconds prd)
      flushSpans mbEpt tracer req mgr -- Manager is thread-safe.
  pure $ Zipkin mgr req tracer mbEpt

-- | Runs a 'TraceT' action, sampling spans appropriately. Note that this method does not publish
-- spans on its own; to do so, either call 'publish' manually or specify a positive
-- 'settingsPublishPeriod' to publish in the background.
run :: TraceT m a -> Zipkin -> m a
run actn zipkin = runTraceT actn (zipkinTracer zipkin)

-- | Flushes all complete spans to the Zipkin server.
publish :: MonadIO m => Zipkin -> m ()
publish z =
  liftIO $ flushSpans (zipkinEndpoint z) (zipkinTracer z) (zipkinRequest z) (zipkinManager z)

-- | Convenience method to start a 'Zipkin', run an action, and publish all spans before returning.
with :: MonadUnliftIO m => Settings -> (Zipkin -> m a) -> m a
with settings f = do
  zipkin <- new settings
  f zipkin `finally` publish zipkin

-- | Adds a tag to the active span.
tag :: MonadTrace m => Text -> Text -> m ()
tag key val = addSpanEntry (publicKeyPrefix <> key) (tagTextValue val)

-- | Adds a tag to a builder. This is a convenience method to use with 'childSpanWith', for example:
--
-- > childSpanWith (addTag "key" "value") "run" $ action
--
-- Note that there is no difference with adding the tag after the span. So the above code is
-- equivalent to:
--
-- > childSpan "run" $ tag "key" "value" >> action
addTag :: Text -> Text -> Builder -> Builder
addTag key val bldr =
  bldr { builderTags = Map.insert (publicKeyPrefix <> key) (JSON.toJSON val) (builderTags bldr) }

-- | Adds a consumer kind tag to a builder. This is a convenience method to use with 'rootSpanWith',
-- for example:
--
-- > rootSpanWith addConsumerKind alwaysSampled "root" $ action
--
-- Use this method if you want to create a root consumer span. Otherwise use 'consumerSpanWith' to
-- create a sub span with consumer kind.
addConsumerKind :: Builder -> Builder
addConsumerKind = appEndo $ insertTag kindKey consumerKindValue

-- | Adds a producer kind tag to a builder. This is a convenience method to use with 'rootSpanWith',
-- for example:
--
-- > rootSpanWith addProducerKind alwaysSampled "root" $ action
--
-- Use this method if you want to create a root producer span. Otherwise use 'producerSpanWith' to
-- create a sub span with producer kind.
addProducerKind :: Builder -> Builder
addProducerKind = appEndo $ insertTag kindKey producerKindValue

-- | Adds an inherited tag to a builder. Unlike a tag added via 'addTag', this tag:
--
-- * will be inherited by all the span's /local/ children.
-- * can only be added at span construction time.
--
-- For example, to add an ID tag to all spans inside a trace:
--
-- > rootSpanWith (addInheritedTag "id" "abcd-efg") alwaysSampled "run" $ action
addInheritedTag :: Text -> Text -> Builder -> Builder
addInheritedTag key val bldr =
  let bgs = builderBaggages bldr
  in bldr { builderBaggages = Map.insert key (T.encodeUtf8 val) bgs }

-- | Annotates the active span using the current time.
annotate :: MonadTrace m => Text -> m ()
annotate val = addSpanEntry "" (logValue val)

-- | Annotates the active span at the given time.
annotateAt :: MonadTrace m => POSIXTime -> Text -> m ()
annotateAt time val = addSpanEntry "" (logValueAt time val)

-- | Exportable trace information, used for cross-process traces.
data B3 = B3
  { b3TraceID :: !TraceID
  -- ^ The span's trace ID.
  , b3SpanID :: !SpanID
  -- ^ The span's ID.
  , b3IsSampled :: !Bool
  -- ^ Whether the span was sampled.
  , b3IsDebug :: !Bool
  -- ^ Whether the span has debug enabled (which implies that the span is sampled).
  , b3ParentSpanID :: !(Maybe SpanID)
  -- ^ The span's parent's ID, or 'Nothing' for root spans.
  } deriving (Eq, Ord, Show)

traceIDHeader, spanIDHeader, parentSpanIDHeader, sampledHeader, debugHeader :: CI ByteString
traceIDHeader = "X-B3-TraceId"
spanIDHeader = "X-B3-SpanId"
parentSpanIDHeader = "X-B3-ParentSpanId"
sampledHeader = "X-B3-Sampled"
debugHeader = "X-B3-Flags"

-- | Serializes the 'B3' to multiple headers, suitable for HTTP requests. All byte-strings are UTF-8
-- encoded.
b3ToHeaders :: B3 -> Map (CI ByteString) ByteString
b3ToHeaders (B3 traceID spanID isSampled isDebug mbParentID) =
  let
    defaultKVs = [(traceIDHeader, encodeZipkinTraceID traceID), (spanIDHeader, encodeSpanID spanID)]
    parentKVs = (parentSpanIDHeader,) . encodeSpanID <$> maybeToList mbParentID
    sampledKVs = case (isSampled, isDebug) of
      (_, True) -> [(debugHeader, "1")]
      (True, _) -> [(sampledHeader, "1")]
      (False, _) -> [(sampledHeader, "0")]
  in fmap T.encodeUtf8 $ Map.fromList $ defaultKVs ++ parentKVs ++ sampledKVs

-- | Deserializes the 'B3' from multiple headers.
b3FromHeaders :: Map (CI ByteString) ByteString -> Maybe B3
b3FromHeaders hdrs = do
  let
    find key = T.decodeUtf8 <$> Map.lookup key hdrs
    findBool def key = case find key of
      Nothing -> Just def
      Just "1" -> Just True
      Just "0" -> Just False
      _ -> Nothing
  dbg <- findBool False debugHeader
  sampled <- findBool dbg sampledHeader
  guard (not $ sampled == False && dbg)
  B3
    <$> (find traceIDHeader >>= decodeZipkinTraceID)
    <*> (find spanIDHeader >>= decodeSpanID)
    <*> pure sampled
    <*> pure dbg
    <*> maybe (pure Nothing) (Just <$> decodeSpanID) (find parentSpanIDHeader)

-- | Serializes the 'B3' to a single UTF-8 encoded header value. It will typically be set as
-- <https://github.com/apache/incubator-zipkin-b3-propagation#single-header b3 header>.
b3ToHeaderValue :: B3 -> ByteString
b3ToHeaderValue (B3 traceID spanID isSampled isDebug mbParentID) =
  let
    state = case (isSampled, isDebug) of
      (_ , True) -> "d"
      (True, _) -> "1"
      (False, _) -> "0"
    required = [encodeZipkinTraceID traceID, encodeSpanID spanID, state]
    optional = encodeSpanID <$> maybeToList mbParentID
  in BS.intercalate "-" $ fmap T.encodeUtf8 $ required ++ optional

-- | Prefix used to fill up 128-bit if only a 64-bit trace identifier is given.
shortTraceIDPrefix :: Text
shortTraceIDPrefix = "0000000000000000"

-- | Decodes a zipkin trace ID from a hex-encoded string, returning nothing if it is invalid. Takes
-- into account that the provided string could be a 16 or 32 lower-hex character trace ID. If the
-- given string consists of 16 lower-hex characters 'shortTraceIDPrefix' is used to fil up the
-- 128-bit trace identifier of 'TraceID'.
decodeZipkinTraceID :: Text -> Maybe TraceID
decodeZipkinTraceID txt =
  let normalized = if T.length txt == 16 then shortTraceIDPrefix <> txt else txt
  in decodeTraceID normalized

-- | Hex-encodes a trace ID, providing a 16 or 32 lower-hex character zipkin trace ID. A 16
-- lower-hex character string is returned if the first 64-bits of the 'TraceID' are zeros.
encodeZipkinTraceID :: TraceID -> Text
encodeZipkinTraceID traceID =
  let txt = encodeTraceID traceID
  in fromMaybe txt $ T.stripPrefix shortTraceIDPrefix txt

-- | Deserializes a single header value into a 'B3'.
b3FromHeaderValue :: ByteString -> Maybe B3
b3FromHeaderValue bs = case T.splitOn "-" $ T.decodeUtf8 bs of
  (traceIDstr:spanIDstr:strs) -> do
    traceID <- decodeZipkinTraceID traceIDstr
    spanID <- decodeSpanID spanIDstr
    let buildB3 = B3 traceID spanID
    case strs of
      [] -> pure $ buildB3 False False Nothing
      (state:strs') -> do
        buildB3' <- case state of
          "0" -> pure $ buildB3 False False
          "1" -> pure $ buildB3 True False
          "d" -> pure $ buildB3 True True
          _ -> Nothing
        case strs' of
          [] -> pure $ buildB3' Nothing
          [str] -> buildB3' . Just <$> decodeSpanID str
          _ -> Nothing
  _ -> Nothing

b3FromSpan :: Span -> B3
b3FromSpan s =
  let
    ctx = spanContext s
    refs = spanReferences s
  in B3 (contextTraceID ctx) (contextSpanID ctx) (spanIsSampled s) (spanIsDebug s) (parentID refs)

-- Builder endos

insertTag :: JSON.ToJSON a => Key -> a -> Endo Builder
insertTag key val =
  Endo $ \bldr -> bldr { builderTags = Map.insert key (JSON.toJSON val) (builderTags bldr) }

importB3 :: B3 -> Endo Builder
importB3 b3 =
  let
    policy = if b3IsDebug b3
      then debugEnabled
      else sampledWhen $ b3IsSampled b3
  in Endo $ \bldr -> bldr
    { builderTraceID = Just (b3TraceID b3)
    , builderSamplingPolicy = Just policy }

-- Prefix added to all user tags. This protects against collisions with internal tags.
publicKeyPrefix :: Text
publicKeyPrefix = "Z."

-- Remote endpoint tag key.
endpointKey :: Key
endpointKey = "z.e"

-- Kind tag key.
kindKey :: Key
kindKey = "z.k"

-- Value that indicates a consumer span kind.
consumerKindValue :: Text
consumerKindValue = "CONSUMER"

-- Value that indicates a producer span kind.
producerKindValue :: Text
producerKindValue = "PRODUCER"

outgoingSpan :: MonadTrace m => Text -> Endo Builder -> Name -> (Maybe B3 -> m a) -> m a
outgoingSpan kind endo name f = childSpanWith (appEndo endo') name actn where
  endo' = insertTag kindKey kind <> endo
  actn = activeSpan >>= \case
    Nothing -> f Nothing
    Just spn -> f $ Just $ b3FromSpan spn

-- | Generates a child span with @CLIENT@ kind. This function also provides the corresponding 'B3'
-- (or 'Nothing' if tracing is inactive) so that it can be forwarded to the server. For example, to
-- emit an HTTP request and forward the trace information in the headers:
--
-- > import Network.HTTP.Simple
-- >
-- > clientSpan "api-call" $ \(Just b3) -> $ do
-- >   res <- httpBS "http://host/api" & addRequestHeader "b3" (b3ToHeaderValue b3)
-- >   process res -- Do something with the response.
clientSpan :: MonadTrace m => Name -> (Maybe B3 -> m a) -> m a
clientSpan = clientSpanWith id

-- | Generates a client span, optionally modifying the span's builder. This can be useful in
-- combination with 'addEndpoint' if the remote server does not have tracing enabled.
clientSpanWith :: MonadTrace m => (Builder -> Builder) -> Name -> (Maybe B3 -> m a) -> m a
clientSpanWith f = outgoingSpan "CLIENT" (Endo f)

-- | Generates a child span with @PRODUCER@ kind. This function also provides the corresponding 'B3'
-- so that it can be forwarded to the consumer.
producerSpanWith :: MonadTrace m => (Builder -> Builder) -> Name -> (Maybe B3 -> m a) -> m a
producerSpanWith f = outgoingSpan producerKindValue (Endo f)

incomingSpan :: MonadTrace m => Text -> B3 -> Endo Builder -> m a -> m a
incomingSpan kind b3 endo actn =
  let bldr = appEndo (insertTag kindKey kind <> importB3 b3 <> endo) $ builder ""
  in trace bldr actn

-- | Generates a child span with @SERVER@ kind. The client's 'B3' should be provided as input,
-- for example parsed using 'b3FromHeaders'.
serverSpan :: MonadTrace m => B3 -> m a -> m a
serverSpan = serverSpanWith id

-- | Generates a child span with @SERVER@ kind, optionally modifying the span's builder. This can
-- be useful in combination with 'addEndpoint' if the remote client does not have tracing enabled.
-- The clients's 'B3' should be provided as input. Client and server annotations go on the same
-- span - it means that they share their span ID.
serverSpanWith :: MonadTrace m => (Builder -> Builder) -> B3 -> m a -> m a
serverSpanWith f b3 =
  let endo = Endo $ \bldr -> f $ bldr { builderSpanID = Just (b3SpanID b3) }
  in incomingSpan "SERVER" b3 endo

-- | Generates a child span with @CONSUMER@ kind, optionally modifying the span's builder. The
-- producer's 'B3' should be provided as input. The generated span will have its parent ID set to
-- the input B3's span ID.
consumerSpanWith :: MonadTrace m => (Builder -> Builder) -> B3 -> m a -> m a
consumerSpanWith f b3 =
  let endo = Endo $ \bldr -> f $ bldr { builderReferences = Set.singleton (ChildOf $ b3SpanID b3) }
  in incomingSpan consumerKindValue b3 endo

-- | Information about a hosted service, included in spans and visible in the Zipkin UI.
data Endpoint = Endpoint
  { endpointService :: !(Maybe Text)
  -- ^ The endpoint's service name.
  , endpointPort :: !(Maybe Int)
  -- ^ The endpoint's port, if applicable and known.
  , endpointIPv4 :: !(Maybe Text)
  -- ^ The endpoint's IPv4 address.
  , endpointIPv6 :: !(Maybe Text)
  -- ^ The endpoint's IPv6 address.
  } deriving (Eq, Ord, Show)

-- | An empty endpoint.
defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint Nothing Nothing Nothing Nothing

-- | Adds a remote endpoint to a builder. This is mostly useful when generating cross-process spans
-- where the remote endpoint is not already traced (otherwise Zipkin will associate the spans
-- correctly automatically). For example when emitting a request to an outside server:
--
-- > clientSpanWith (addEndpoint "outside-api") -- ...
addEndpoint :: Endpoint -> Builder -> Builder
addEndpoint = appEndo . insertTag endpointKey

-- | Generates an endpoint with the given string as service.
instance IsString Endpoint where
  fromString s = defaultEndpoint { endpointService = Just (T.pack s) }

instance JSON.ToJSON Endpoint where
  toJSON (Endpoint mbSvc mbPort mbIPv4 mbIPv6) = JSON.object $ catMaybes
    [ ("serviceName" JSON..=) <$> mbSvc
    , ("port" JSON..=) <$> mbPort
    , ("ipv4" JSON..=) <$> mbIPv4
    , ("ipv6" JSON..=) <$> mbIPv6 ]

parentID :: Set Reference -> Maybe SpanID
parentID = listToMaybe . catMaybes . fmap go . toList where
  go (ChildOf d) = Just d
  go _ = Nothing

data ZipkinAnnotation = ZipkinAnnotation !POSIXTime !JSON.Value

instance JSON.ToJSON ZipkinAnnotation where
  toJSON (ZipkinAnnotation t v) = JSON.object
    [ "timestamp" JSON..= microSeconds @Int64 t
    , "value" JSON..= v ]

-- Internal type used to encode spans in the <https://zipkin.apache.org/zipkin-api/#/ format>
-- expected by Zipkin.
data ZipkinSpan = ZipkinSpan !(Maybe Endpoint) !Sample

publicTags :: Tags -> Map Text JSON.Value
publicTags = Map.fromList . catMaybes . fmap go . Map.assocs where
  go (k, v) = case T.stripPrefix publicKeyPrefix k of
    Nothing -> Nothing
    Just k' -> Just (k', v)

instance JSON.ToJSON ZipkinSpan where
  toJSON (ZipkinSpan mbEpt (Sample spn tags logs start duration)) =
    let
      ctx = spanContext spn
      requiredKVs =
        [ "traceId" JSON..= contextTraceID ctx
        , "name" JSON..= spanName spn
        , "id" JSON..= contextSpanID ctx
        , "timestamp" JSON..= microSeconds @Int64 start
        , "duration" JSON..= microSeconds @Int64 duration
        , "debug" JSON..= spanIsDebug spn
        , "tags" JSON..= (publicTags tags <> (JSON.toJSON . T.decodeUtf8 <$> contextBaggages ctx))
        , "annotations" JSON..= fmap (\(t, _, v) -> ZipkinAnnotation t v) logs ]
      optionalKVs = catMaybes
        [ ("parentId" JSON..=) <$> parentID (spanReferences spn)
        , ("localEndpoint" JSON..=) <$> mbEpt
        , ("remoteEndpoint" JSON..=) <$> Map.lookup endpointKey tags
        , ("kind" JSON..=) <$> Map.lookup kindKey tags ]
    in JSON.object $ requiredKVs ++ optionalKVs

microSeconds :: Integral a => NominalDiffTime -> a
microSeconds = round . (* 1000000)
