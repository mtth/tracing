{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-| Zipkin trace publisher. -}
module Monitor.Tracing.Zipkin (
  -- * Set up the trace collector
  Zipkin, new, Settings(..), defaultSettings, Endpoint(..), defaultEndpoint, run, publish,
  -- * Record in-process spans
  rootSpan, Sampling(..), localSpan,
  -- * Record cross-process spans
  B3, clientSpan, serverSpan, producerSpan, consumerSpan,
  -- * Add metadata
  tag, annotate, annotateAt
) where

import Control.Monad.Trace
import Control.Monad.Trace.Class

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, tryReadTChan)
import Control.Monad (forever, void, when)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import Data.Time.Clock (NominalDiffTime)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, maybe, maybeToList)
import Data.Monoid (Endo(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Net.IPv4 (IPv4)
import Net.IPv6 (IPv6)
import Network.HTTP.Client (Manager, Request)
import qualified Network.HTTP.Client as HTTP

-- | Zipkin creating settings.
data Settings = Settings
  { settingsManager :: !(Maybe Manager)
  -- ^ An optional HTTP manager to use for publishing spans on the Zipkin server.
  , settingsHost :: !ByteString
  -- ^ The Zipkin server host.
  , settingsPort :: !Int
  -- ^ The port the Zipkin server is listening on.
  , settingsEndpoint :: !(Maybe Endpoint)
  -- ^ Local endpoint used for all published spans.
  , settingsPublishPeriod :: !NominalDiffTime
  -- ^ If set to a positive value, traces will be flushed in the background every such period.
  }

-- | Creates 'Settings' pointing to a Zikpin server at host @"localhost"@ and port @9411@, without
-- background flushing.
defaultSettings :: Settings
defaultSettings = Settings Nothing "localhost" 9411 Nothing 0

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
      when (isSampled spn) $ modifyIORef ref (ZipkinSpan ept spn tags logs itv:)
      loop
  spns <- readIORef ref
  let req' = req { HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode spns }
  void $ HTTP.httpLbs req' mgr

-- | Creates a 'Zipkin' publisher for the input 'Settings'.
new :: MonadIO m => Settings -> m Zipkin
new (Settings mbMgr host port mbEpt prd) = liftIO $ do
  mgr <- maybe (HTTP.newManager HTTP.defaultManagerSettings) pure mbMgr
  tracer <- newTracer
  let
    req = HTTP.defaultRequest
      { HTTP.method = "POST"
      , HTTP.host = host
      , HTTP.path = "/api/v2/spans"
      , HTTP.port = port }
  void $ if prd <= 0
    then pure Nothing
    else fmap Just $ forkIO $ forever $ do
      threadDelay (microSeconds prd)
      flushSpans mbEpt tracer req mgr -- Manager is thread-safe.
  pure $ Zipkin mgr req tracer mbEpt

-- | Runs a 'TraceT' action, sampling spans appropriately. Note that this method does not publish
-- spans on its own; to do so, either call 'publish' manually or specify a positive
-- 'settingsPublishPeriod' to publish in the background.
run :: Zipkin -> TraceT m a -> m a
run zipkin actn = runTraceT actn (zipkinTracer zipkin)

-- | Flushes all complete spans to the Zipkin server. This method is thread-safe.
publish :: MonadIO m => Zipkin -> m ()
publish z =
  liftIO $ flushSpans (zipkinEndpoint z) (zipkinTracer z) (zipkinRequest z) (zipkinManager z)

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
  , b3Sampling :: !(Maybe Sampling)
  } deriving (Eq, Show)

b3FromSpan :: Span -> B3
b3FromSpan s =
  let
    ctx = spanContext s
    samplingState = if fromMaybe False (lookupBoolBaggage debugKey ctx)
      then Just Debug
      else explicitSamplingState <$> lookupBoolBaggage samplingKey ctx
  in B3 (contextTraceID ctx) (contextSpanID ctx) (parentID $ spanReferences s) samplingState

-- Builder endos

insertBaggage :: Key -> ByteString -> Endo Builder
insertBaggage key val =
  Endo $ \bldr -> bldr { builderBaggages = Map.insert key val (builderBaggages bldr) }

insertTag :: JSON.ToJSON a => Key -> a -> Endo Builder
insertTag key val =
  Endo $ \bldr -> bldr { builderTags = Map.insert key (JSON.toJSON val) (builderTags bldr) }

importB3 :: B3 -> Endo Builder
importB3 b3 =
  let
    baggages = Map.fromList $ case b3Sampling b3 of
      Just Accept -> [(samplingKey, "1")]
      Just Deny -> [(samplingKey, "0")]
      Just Debug -> [(debugKey, "1")]
      Nothing -> []
  in Endo $ \bldr -> bldr
    { builderTraceID = Just (b3TraceID b3)
    , builderSpanID = Just (b3SpanID b3)
    , builderBaggages = baggages }

-- | The sampling applied to a trace.
--
-- Note that non-sampled traces still yield active spans. However these spans are not published to
-- Zipkin.
data Sampling
  = Accept
  -- ^ Sample it.
  | Debug
  -- ^ Sample it and mark it as debug.
  | Deny
  -- ^ Do not sample it.
  deriving (Eq, Ord, Enum, Show)

applySampling :: Sampling -> Endo Builder
applySampling Accept = insertBaggage samplingKey "1"
applySampling Debug = insertBaggage debugKey "1"
applySampling Deny = insertBaggage samplingKey "0"

isSampled :: Span -> Bool
isSampled spn =
  let ctx = spanContext spn
  in fromMaybe False $ lookupBoolBaggage debugKey ctx <|> lookupBoolBaggage samplingKey ctx

publicKeyPrefix :: Text
publicKeyPrefix = "Z."

-- Debug baggage key.
debugKey :: Key
debugKey = "z.d"

-- Sampling baggage key.
samplingKey :: Key
samplingKey = "z.s"

-- Remote endpoint tag key.
endpointKey :: Key
endpointKey = "z.e"

-- Kind tag key.
kindKey :: Key
kindKey = "z.k"

-- Internal keys

-- | Starts a new trace.
rootSpan :: MonadTrace m => Sampling -> Name -> m a -> m a
rootSpan sampling name = trace $ appEndo (applySampling sampling) $ builder name

childSpan :: MonadTrace m => Endo Builder -> Name -> m a -> m a
childSpan endo name actn = activeSpan >>= \case
  Nothing -> actn
  Just spn -> do
    let
      ctx = spanContext spn
      bldr = (builder name)
        { builderTraceID = Just $ contextTraceID ctx
        , builderReferences = Set.singleton (ChildOf $ contextSpanID ctx) }
    trace (appEndo endo $ bldr) actn

-- | Continues an existing trace if present, otherwise does nothing.
localSpan :: MonadTrace m => Name -> m a -> m a
localSpan = childSpan mempty

outgoingSpan :: MonadTrace m => Text -> Maybe Endpoint -> Name -> (Maybe B3 -> m a) -> m a
outgoingSpan kind mbEpt name f = childSpan endo name actn where
  endo = insertTag kindKey kind <> maybe mempty (insertTag endpointKey) mbEpt
  actn = activeSpan >>= \case
    Nothing -> f Nothing
    Just spn -> f $ Just $ b3FromSpan spn

clientSpan :: MonadTrace m => Maybe Endpoint -> Name -> (Maybe B3 -> m a) -> m a
clientSpan = outgoingSpan "CLIENT"

producerSpan :: MonadTrace m => Maybe Endpoint -> Name -> (Maybe B3 -> m a) -> m a
producerSpan = outgoingSpan "PRODUCER"

incomingSpan :: MonadTrace m => Text -> Maybe Endpoint -> B3 -> m a -> m a
incomingSpan kind mbEpt b3 actn =
  let
    endo = importB3 b3 <> insertTag kindKey kind <> maybe mempty (insertTag endpointKey) mbEpt
    bldr = appEndo endo $ builder ""
  in trace bldr actn

serverSpan :: MonadTrace m => Maybe Endpoint -> B3 -> m a -> m a
serverSpan = incomingSpan "SERVER"

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

explicitSamplingState :: Bool -> Sampling
explicitSamplingState True = Accept
explicitSamplingState False = Deny

lookupBoolBaggage :: Key -> Context -> Maybe Bool
lookupBoolBaggage key ctx = Map.lookup key (contextBaggages ctx) >>= \case
  "0" -> Just False
  "1" -> Just True
  _ -> Nothing

parentID :: Set Reference -> Maybe SpanID
parentID = listToMaybe . catMaybes . fmap go . toList where
  go (ChildOf d) = Just d
  go _ = Nothing

traceIDHeader, spanIDHeader, parentSpanIDHeader, sampledHeader, debugHeader :: Text
traceIDHeader = "X-B3-TraceId"
spanIDHeader = "X-B3-SpanId"
parentSpanIDHeader = "X-B3-ParentSpanId"
sampledHeader = "X-B3-Sampled"
debugHeader = "X-B3-Flags"

instance JSON.FromJSON B3 where
  parseJSON = JSON.withObject "B3" $ \v -> do
    dbg <- v JSON..:! debugHeader JSON..!= False
    state <- fmap explicitSamplingState <$> v JSON..:! sampledHeader
    let
      sampling = case (dbg, state) of
        (False, s) -> pure s
        (True, Nothing) -> pure (Just Debug)
        _ -> fail "bad debug and sampling combination"
    B3
      <$> v JSON..: traceIDHeader
      <*> v JSON..: spanIDHeader
      <*> v JSON..:! parentSpanIDHeader
      <*> sampling

instance JSON.ToJSON B3 where
  toJSON (B3 traceID spanID mbParentID state) =
    let
      defaultKVs = [traceIDHeader JSON..= traceID, spanIDHeader JSON..= spanID]
      parentKVs = (parentSpanIDHeader JSON..=) <$> maybeToList mbParentID
      sampledKVs = case state of
        Nothing -> []
        Just Debug -> [debugHeader JSON..= ("1" :: Text)]
        Just Accept -> [sampledHeader JSON..= ("1" :: Text)]
        Just Deny -> [sampledHeader JSON..= ("0" :: Text)]
    in JSON.object $ defaultKVs ++ parentKVs ++ sampledKVs

data ZipkinAnnotation = ZipkinAnnotation !POSIXTime !JSON.Value

instance JSON.ToJSON ZipkinAnnotation where
  toJSON (ZipkinAnnotation t v) = JSON.object
    [ "timestamp" JSON..= microSeconds @Int64 t
    , "value" JSON..= v ]

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
        , "debug" JSON..= fromMaybe False (lookupBoolBaggage debugKey ctx)
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
