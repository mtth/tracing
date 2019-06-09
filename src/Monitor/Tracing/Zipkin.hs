{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-| <https://zipkin.apache.org/ Zipkin> trace publisher. -}
module Monitor.Tracing.Zipkin (
  -- * Set up the trace collector
  Zipkin,
  new, Settings(..), defaultSettings, Endpoint(..), defaultEndpoint,
  run, publish, with,
  -- * Record cross-process spans
  B3, b3FromHeaders, b3ToHeaders, clientSpan, serverSpan, producerSpan, consumerSpan,
  -- * Add metadata
  tag, annotate, annotateAt
) where

import Control.Monad.Trace
import Control.Monad.Trace.Class

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad (forever, guard, void, when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (NominalDiffTime)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe, maybe, maybeToList)
import Data.Monoid (Endo(..))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Net.IPv4 (IPv4)
import Net.IPv6 (IPv6)
import Network.HTTP.Client (Manager, Request)
import qualified Network.HTTP.Client as HTTP
import Network.Socket (HostName, PortNumber)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (finally)

-- | Zipkin creating settings.
data Settings = Settings
  { settingsHostname :: !HostName
  -- ^ The Zipkin server's hostname.
  , settingsPort :: !PortNumber
  -- ^ The port the Zipkin server is listening on.
  , settingsEndpoint :: !(Maybe Endpoint)
  -- ^ Local endpoint used for all published spans.
  , settingsManager :: !(Maybe Manager)
  -- ^ An optional HTTP manager to use for publishing spans on the Zipkin server.
  , settingsPublishPeriod :: !NominalDiffTime
  -- ^ If set to a positive value, traces will be flushed in the background every such period.
  }

-- | Creates 'Settings' pointing to a Zikpin server at host @"localhost"@ and port @9411@, without
-- background flushing.
defaultSettings :: Settings
defaultSettings = Settings "localhost" 9411 Nothing Nothing 0

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
    Just (spn, tags, logs, itv) -> do
      when (spanIsSampled spn) $ modifyIORef ref (ZipkinSpan ept spn tags logs itv:)
      loop
  spns <- readIORef ref
  let req' = req { HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode spns }
  void $ HTTP.httpLbs req' mgr

-- | Creates a 'Zipkin' publisher for the input 'Settings'.
new :: MonadIO m => Settings -> m Zipkin
new (Settings hostname port mbEpt mbMgr prd) = liftIO $ do
  mgr <- maybe (HTTP.newManager HTTP.defaultManagerSettings) pure mbMgr
  tracer <- newTracer
  let
    req = HTTP.defaultRequest
      { HTTP.method = "POST"
      , HTTP.host = BS.pack hostname
      , HTTP.path = "/api/v2/spans"
      , HTTP.port = fromIntegral port }
  void $ if prd <= 0
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
  , b3ParentSpanID :: !(Maybe SpanID)
  , b3IsSampled :: !Bool
  , b3IsDebug :: !Bool
  } deriving (Eq, Show)

traceIDHeader, spanIDHeader, parentSpanIDHeader, sampledHeader, debugHeader :: Text
traceIDHeader = "X-B3-TraceId"
spanIDHeader = "X-B3-SpanId"
parentSpanIDHeader = "X-B3-ParentSpanId"
sampledHeader = "X-B3-Sampled"
debugHeader = "X-B3-Flags"

-- | Serialize the 'B3' to headers, suitable for HTTP requests.
b3ToHeaders :: B3 -> Map Text Text
b3ToHeaders (B3 traceID spanID mbParentID isSampled isDebug) =
  let
    defaultKVs = [(traceIDHeader, encodeTraceID traceID), (spanIDHeader, encodeSpanID spanID)]
    parentKVs = (parentSpanIDHeader,) . encodeSpanID <$> maybeToList mbParentID
    sampledKVs = case (isSampled, isDebug) of
      (_, True) -> [(debugHeader, "1")]
      (True, _) -> [(sampledHeader, "1")]
      (False, _) -> [(sampledHeader, "0")]
  in Map.fromList $ defaultKVs ++ parentKVs ++ sampledKVs

-- | Deserialize the 'B3' from headers.
b3FromHeaders :: Map Text Text -> Maybe B3
b3FromHeaders hdrs = do
  let
    find key = Map.lookup key hdrs
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
    <*> maybe (pure Nothing) (Just <$> decodeSpanID) (find parentSpanIDHeader)
    <*> pure sampled
    <*> pure dbg

instance JSON.FromJSON B3 where
  parseJSON = JSON.withObject "B3" $ \v -> do
    hdrs <- JSON.parseJSON (JSON.Object v)
    maybe (fail "bad input") pure $ b3FromHeaders hdrs

instance JSON.ToJSON B3 where
  toJSON = JSON.toJSON . b3ToHeaders

b3FromSpan :: Span -> B3
b3FromSpan s =
  let
    ctx = spanContext s
    refs = spanReferences s
  in B3 (contextTraceID ctx) (contextSpanID ctx) (parentID refs) (spanIsSampled s) (spanIsDebug s)

-- Builder endos

insertTag :: JSON.ToJSON a => Key -> a -> Endo Builder
insertTag key val =
  Endo $ \bldr -> bldr { builderTags = Map.insert key (JSON.toJSON val) (builderTags bldr) }

importB3 :: B3 -> Endo Builder
importB3 b3 =
  let
    sampling = if b3IsDebug b3
      then debugEnabled
      else sampledWhen $ b3IsSampled b3
  in Endo $ \bldr -> bldr
    { builderTraceID = Just (b3TraceID b3)
    , builderSpanID = Just (b3SpanID b3)
    , builderSampling = Just sampling }

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
-- so that it can be forwarded to the server.
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

-- | Generates a child span with @SERVER@ kind. The client's 'B3' should be provided as input.
serverSpan :: MonadTrace m => Maybe Endpoint -> B3 -> m a -> m a
serverSpan = incomingSpan "SERVER"

-- | Generates a child span with @CONSUMER@ kind. The producer's 'B3' should be provided as input.
consumerSpan :: MonadTrace m => Maybe Endpoint -> B3 -> m a -> m a
consumerSpan = incomingSpan "CONSUMER"

-- | Information about a hosted service.
data Endpoint = Endpoint
  { endpointService :: !(Maybe Text)
  , endpointPort :: !(Maybe Int)
  , endpointIPv4 :: !(Maybe IPv4)
  , endpointIPv6 :: !(Maybe IPv6)
  } deriving (Eq, Ord, Show)

-- | An empty endpoint.
defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint Nothing Nothing Nothing Nothing

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
data ZipkinSpan = ZipkinSpan !(Maybe Endpoint) !Span !Tags !Logs !Interval

publicTags :: Tags -> Map Text JSON.Value
publicTags = Map.fromList . catMaybes . fmap go . Map.assocs where
  go (k, v) = case T.stripPrefix publicKeyPrefix k of
    Nothing -> Nothing
    Just k' -> Just (k', v)

instance JSON.ToJSON ZipkinSpan where
  toJSON (ZipkinSpan mbEpt spn tags logs itv) =
    let
      ctx = spanContext spn
      requiredKVs =
        [ "traceId" JSON..= contextTraceID ctx
        , "name" JSON..= spanName spn
        , "id" JSON..= contextSpanID ctx
        , "timestamp" JSON..= microSeconds @Int64 (intervalStart itv)
        , "duration" JSON..= microSeconds @Int64 (intervalDuration itv)
        , "debug" JSON..= spanIsDebug spn
        , "tags" JSON..= publicTags tags
        , "annotations" JSON..= fmap (\(t, _, v) -> ZipkinAnnotation t v) logs ]
      optionalKVs = catMaybes
        [ ("parentId" JSON..=) <$> parentID (spanReferences spn)
        , ("localEndpoint" JSON..=) <$> mbEpt
        , ("remoteEndpoint" JSON..=) <$> Map.lookup endpointKey tags
        , ("kind" JSON..=) <$> Map.lookup kindKey tags ]
    in JSON.object $ requiredKVs ++ optionalKVs

microSeconds :: Integral a => NominalDiffTime -> a
microSeconds = round . (* 1000000)
