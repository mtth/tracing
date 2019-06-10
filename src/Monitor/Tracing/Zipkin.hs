{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This module implements a <https://zipkin.apache.org/ Zipkin>-powered trace publisher. You will
-- almost certainly want to import it qualified.
module Monitor.Tracing.Zipkin (
  -- * Configuration
  -- ** General settings
  Settings, defaultSettings, settingsHostname, settingsPort, settingsManager, settingsEndpoint,
  settingsPublishPeriod,
  -- ** Endpoint
  Endpoint, defaultEndpoint, endpointService, endpointPort, endpointIPv4, endpointIPv6,

  -- * Publishing traces
  Zipkin,
  new, run, publish, with,

  -- * Cross-process spans
  -- ** Communication
  B3, b3ToHeaders, b3FromHeaders, b3ToHeaderValue, b3FromHeaderValue,
  -- ** Span generation
  clientSpan, serverSpan, producerSpan, consumerSpan,

  -- * Custom metadata
  -- ** Tags
  tag, addTag, addInheritedTag,
  -- ** Annotations
  -- | Annotations are similar to tags, but timestamped.
  annotate, annotateAt
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
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, maybe, maybeToList)
import Data.Monoid ((<>), Endo(..))
import Data.Set (Set)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX (POSIXTime)
import Net.IPv4 (IPv4)
import Net.IPv6 (IPv6)
import Network.HTTP.Client (Manager, Request)
import qualified Network.HTTP.Client as HTTP
import Network.Socket (HostName, PortNumber)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (finally)

-- | 'Zipkin' creation settings. Note that its constructor is not exposed to allow backwards
-- compatible evolution; 'Settings' should instead be created either via 'defaultSettings' or its
-- 'IsString' instance.
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
data Zipkin = Zipkin
  { zipkinManager :: !Manager
  , zipkinRequest :: !Request
  , zipkinTracer :: !Tracer
  , zipkinEndpoint :: !(Maybe Endpoint)
  }

flushSpans :: Maybe Endpoint -> Tracer -> Request -> Manager -> IO ()
flushSpans ept tracer req mgr = do
  ref <- newIORef []
  fix $ \loop -> atomically (tryReadTChan $ tracerChannel tracer) >>= \case
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
      , HTTP.port = maybe 9411 fromIntegral mbPort }
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

-- | Flushes all complete spans to the Zipkin server. This method is thread-safe.
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
-- Note that there is not difference with adding the tag after the span. So the above code is
-- equivalent to:
--
-- > childSpan "run" $ tag "key" "value" >> action
addTag :: Text -> Text -> Builder -> Builder
addTag key val bldr = bldr { builderTags = Map.insert key (JSON.toJSON val) (builderTags bldr) }

-- | Adds an inherited tag to a builder. Unlike a tag added via 'addTag', this tag:
--
-- * will be inherited by all the span's children.
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
  , b3SpanID :: !SpanID
  , b3IsSampled :: !Bool
  , b3IsDebug :: !Bool
  , b3ParentSpanID :: !(Maybe SpanID)
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
    defaultKVs = [(traceIDHeader, encodeTraceID traceID), (spanIDHeader, encodeSpanID spanID)]
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
    <$> (find traceIDHeader >>= decodeTraceID)
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
    required = [encodeTraceID traceID, encodeSpanID spanID, state]
    optional = encodeSpanID <$> maybeToList mbParentID
  in BS.intercalate "-" $ fmap T.encodeUtf8 $ required ++ optional

-- | Deserializes a single header value into a 'B3'.
b3FromHeaderValue :: ByteString -> Maybe B3
b3FromHeaderValue bs = case T.splitOn "-" $ T.decodeUtf8 bs of
  (traceIDstr:spanIDstr:strs) -> do
    traceID <- decodeTraceID traceIDstr
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
    , builderSpanID = Just (b3SpanID b3)
    , builderSamplingPolicy = Just policy }

publicKeyPrefix :: Text
publicKeyPrefix = "Z."

-- Remote endpoint tag key.
endpointKey :: Key
endpointKey = "z.e"

-- Kind tag key.
kindKey :: Key
kindKey = "z.k"

-- Internal keys

outgoingSpan :: MonadTrace m => Text -> Maybe Endpoint -> Name -> (Maybe B3 -> m a) -> m a
outgoingSpan kind mbEpt name f = childSpanWith (appEndo endo) name actn where
  endo = insertTag kindKey kind <> maybe mempty (insertTag endpointKey) mbEpt
  actn = activeSpan >>= \case
    Nothing -> f Nothing
    Just spn -> f $ Just $ b3FromSpan spn

-- | Generates a child span with @CLIENT@ kind. This function also provides the corresponding 'B3'
-- (or 'Nothing' if tracing is inactive) so that it can be forwarded to the server. For example, to
-- emit an HTTP request and forward the trace information in the headers:
--
-- > clientSpan "api-call" $ \(Just b3) -> $ do
-- >   res <- httpLbs "http://host/api" { requestHeaders = b3ToHeaders b3 }
-- >   process res -- Do something with the response.
clientSpan :: MonadTrace m => Maybe Endpoint -> Name -> (Maybe B3 -> m a) -> m a
clientSpan = outgoingSpan "CLIENT"

-- | Generates a child span with @PRODUCER@ kind. This function also provides the corresponding 'B3'
-- so that it can be forwarded to the consumer.
producerSpan :: MonadTrace m => Maybe Endpoint -> Name -> (Maybe B3 -> m a) -> m a
producerSpan = outgoingSpan "PRODUCER"

incomingSpan :: MonadTrace m => Text -> Maybe Endpoint -> B3 -> m a -> m a
incomingSpan kind mbEpt b3 actn =
  let
    endo = importB3 b3 <> insertTag kindKey kind <> maybe mempty (insertTag endpointKey) mbEpt
    bldr = appEndo endo $ builder ""
  in trace bldr actn

-- | Generates a child span with @SERVER@ kind. The client's 'B3' should be provided as input,
-- for example parsed using 'b3FromHeaders'.
serverSpan :: MonadTrace m => Maybe Endpoint -> B3 -> m a -> m a
serverSpan = incomingSpan "SERVER"

-- | Generates a child span with @CONSUMER@ kind. The producer's 'B3' should be provided as input.
consumerSpan :: MonadTrace m => Maybe Endpoint -> B3 -> m a -> m a
consumerSpan = incomingSpan "CONSUMER"

-- | Information about a hosted service, included in spans and visible in the Zipkin UI.
data Endpoint = Endpoint
  { endpointService :: !(Maybe Text)
  -- ^ The endpoint's service name.
  , endpointPort :: !(Maybe Int)
  -- ^ The endpoint's port, if applicable and known.
  , endpointIPv4 :: !(Maybe IPv4)
  -- ^ The endpoint's IPv4 address.
  , endpointIPv6 :: !(Maybe IPv6)
  -- ^ The endpoint's IPv6 address.
  } deriving (Eq, Ord, Show)

-- | An empty endpoint.
defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint Nothing Nothing Nothing Nothing

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
