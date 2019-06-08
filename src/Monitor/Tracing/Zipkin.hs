{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Zipkin trace publisher.
--
-- Limitations:
--
-- * Only one parent is supported per span (if multiple are specified, an arbitrary one will be
-- selected).
--
-- run :: Trace ()
-- run = rootSpan defaultFlags "run" $ do
--   runA
--   runB
--
-- -- TODO: Use HasStackTrace to implement a variant which infers the name?
-- runA :: Trace ()
-- runA = localSpan "runA" $ do
--   print "hi"
--
-- runB :: Trace ()
-- runB = clientSpan Nothing "runB" $ \b3 -> do
--   http b3
--
-- zipkinClient :: IO ()
-- zipkinClient = do
--   zipkin <- startZipkin
--   publishSpans zipkin run
--   flushSpans zipkin
--
-- zipkinServerHandler :: Handler Int
-- zipkinServerHandler = b3Headers >>= serverSpan Nothing $ do
--   pure 1
module Monitor.Tracing.Zipkin
  ( Zipkin, newZipkin, publishSpans, flushSpans
  , ZipkinSettings(..), defaultZipkinSettings
  , B3, b3FromActiveSpan
  , traceIDHeader, spanIDHeader, parentSpanIDHeader, sampledHeader, debugHeader
  , rootSpan, localSpan
  , tagSpan, annotateSpan, annotateSpanAt
  , Flags, defaultFlags, accepted, denied, debug
  , Endpoint(..), endpoint
  , MonadTrace, tracedFork
  ) where

import Control.Monad.Trace
import Control.Monad.Trace.Class
import Monitor.Tracing
import Monitor.Tracing.Internal

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM (atomically, tryReadTChan)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, void)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (NominalDiffTime)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
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

data ZipkinSettings = ZipkinSettings
  { zipkinSettingsManager :: !(Maybe Manager)
  , zipkinSettingsHost :: !ByteString
  , zipkinSettingsPort :: !Int
  , zipkinSettingsEndpoint :: !(Maybe Endpoint) -- Local endpoint.
  , zipkinSettingsFlushPeriod :: !NominalDiffTime
  }

defaultZipkinSettings :: ZipkinSettings
defaultZipkinSettings = ZipkinSettings Nothing "localhost" 9411 Nothing 30

data Zipkin = Zipkin
  { _zipkinManager :: !Manager
  , _zipkinRequest :: !Request
  , _zipkinTracer :: !Tracer
  , _zipkinEndpoint :: !(Maybe Endpoint)
  , _zipkinThreadID :: !(Maybe ThreadId)
  }

newZipkin :: MonadIO m => ZipkinSettings -> m Zipkin
newZipkin (ZipkinSettings mgrQ host port ept prd) = liftIO $ do
  mgr <- maybe (HTTP.newManager HTTP.defaultManagerSettings) pure mgrQ
  tracer <- newTracer
  let
    req = HTTP.defaultRequest
      { HTTP.method = "POST"
      , HTTP.host = host
      , HTTP.path = "/api/v2/spans"
      , HTTP.port = port
      }
  threadIDQ <- if prd <= 0
    then pure Nothing
    else fmap Just $ forkIO $ forever $ do
      threadDelay (microSeconds prd)
      flush ept tracer req mgr
  pure $ Zipkin mgr req tracer ept threadIDQ

flush :: Maybe Endpoint -> Tracer -> Request -> Manager -> IO ()
flush ept tracer req mgr = do
  ref <- newIORef []
  fix $ \loop -> atomically (tryReadTChan $ tracerChannel tracer) >>= \case
    Nothing -> pure ()
    Just spn -> modifyIORef ref (spn:) >> loop
  spns <- fmap (ZipkinSpan ept) <$> readIORef ref
  let req' = req { HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode spns }
  void $ HTTP.httpLbs req' mgr

publishSpans :: Zipkin -> TraceT m a -> m a
publishSpans zipkin actn = runTraceT actn (_zipkinTracer zipkin)

flushSpans :: MonadIO m => Zipkin -> m ()
flushSpans (Zipkin mgr req tracer ept _) = liftIO $ flush ept tracer req mgr

tagSpan :: MonadTrace m => Text -> Text -> m ()
tagSpan key val = annotate key (tagTextValue val)

annotateSpan :: MonadTrace m => Text -> m ()
annotateSpan val = annotate "" (logValue val)

annotateSpanAt :: MonadTrace m => POSIXTime -> Text -> m ()
annotateSpanAt time val = annotate "" (logValueAt time val)

data B3 = B3
  { b3TraceID :: !TraceID
  , b3SpanID :: !SpanID
  , b3ParentSpanID :: !(Maybe SpanID)
  , b3Sampling :: !(Maybe Sampling)
  } deriving (Eq, Show)

newtype Flags = Flags (Endo Builder) deriving Semigroup

insertBaggage :: Key -> ByteString -> Endo Builder
insertBaggage key val =
  Endo $ \bldr -> bldr { builderBaggages = Map.insert key val (builderBaggages bldr) }

insertTag :: JSON.ToJSON a => Key -> a -> Endo Builder
insertTag key val =
  Endo $ \bldr -> bldr { builderTags = Map.insert key (JSON.toJSON val) (builderTags bldr) }

defaultFlags :: Flags
defaultFlags = Flags mempty

accepted :: Flags
accepted = Flags $ insertBaggage samplingKey "1"

denied :: Flags
denied = Flags $ insertBaggage samplingKey "0"

debug :: Flags
debug = Flags $ insertBaggage debugKey "1"

-- Internal keys

-- Debug baggage key.
debugKey :: Key
debugKey = "__zd"

-- Sampling baggage key.
samplingKey :: Key
samplingKey = "__zs"

-- Remote endpoint tag key.
endpointKey :: Key
endpointKey = "__ze"

-- Kind tag key.
kindKey :: Key
kindKey = "__zk"

rootSpan :: MonadTrace m => Flags -> Name -> m a -> m a
rootSpan (Flags endo) name = trace $ appEndo endo $ builder name

childSpan :: MonadTrace m => Endo Builder -> Name -> m a -> m a
childSpan endo name action = activeSpan >>= \case
  Nothing -> action
  Just spn -> do
    let
      ctx = _activeSpanContext spn
      bldr = (builder name)
        { builderTraceID = Just $ _contextTraceID ctx
        , builderReferences = Set.singleton (ChildOf $ _contextSpanID ctx)
        }
    trace (appEndo endo $ bldr) action

localSpan :: MonadTrace m => Name -> m a -> m a
localSpan = childSpan mempty

clientSpan :: MonadTrace m => Maybe Endpoint -> Name -> (Maybe B3 -> m a) -> m a
clientSpan mbEpt name f = childSpan endo name action where
  endo = insertTag kindKey ("CLIENT" :: Text) <> maybe mempty (insertTag endpointKey) mbEpt
  action = activeSpan >>= \case
    Nothing -> f Nothing
    Just spn -> f $ Just $ b3FromActiveSpan spn

serverSpan :: MonadTrace m => Maybe Endpoint -> B3 -> m a -> m a
serverSpan = undefined

consumerSpan :: MonadTrace m => Maybe Endpoint -> B3 -> m a -> m a
consumerSpan = undefined

-- | Note that childBuilder with Zipkin reuses the span's ID.
childBuilder :: B3 -> Builder
childBuilder b3 =
  let
    baggages = Map.fromList $ case b3Sampling b3 of
      Just Accept -> [(samplingKey, "1")]
      Just Deny -> [(samplingKey, "0")]
      Just Debug -> [(debugKey, "1")]
      Nothing -> []
  in (builder "")
    { builderTraceID = Just (b3TraceID b3)
    , builderSpanID = Just (b3SpanID b3)
    , builderBaggages = baggages
    }

data Endpoint = Endpoint
  { endpointService :: !(Maybe Text)
  , endpointPort :: !(Maybe Int)
  , endpointIPv4 :: !(Maybe IPv4)
  , endpointIPv6 :: !(Maybe IPv6)
  }

endpoint :: Endpoint
endpoint = Endpoint Nothing Nothing Nothing Nothing

instance JSON.ToJSON Endpoint where
  toJSON (Endpoint mbSvc mbPort mbIPv4 mbIPv6) = JSON.object $ catMaybes
    [ ("serviceName" JSON..=) <$> mbSvc
    , ("port" JSON..=) <$> mbPort
    , ("ipv4" JSON..=) <$> mbIPv4
    , ("ipv6" JSON..=) <$> mbIPv6
    ]

data Sampling
  = Accept
  | Deny
  | Debug
  deriving (Eq, Ord, Enum, Show)

explicitSamplingState :: Bool -> Sampling
explicitSamplingState True = Accept
explicitSamplingState False = Deny

lookupBoolBaggage :: Key -> Context -> Maybe Bool
lookupBoolBaggage key ctx = Map.lookup key (_contextBaggages ctx) >>= \case
  "0" -> Just False
  "1" -> Just True
  _ -> Nothing

lookupTag :: Key -> Span -> Maybe JSON.Value
lookupTag key spn = Map.lookup key (_spanTags spn)

parentID :: Set Reference -> Maybe SpanID
parentID s = listToMaybe $ catMaybes $ fmap parentSpanID $ toList s

b3FromActiveSpan :: ActiveSpan -> B3
b3FromActiveSpan s =
  let
    ctx = _activeSpanContext s
    samplingState = if fromMaybe False (lookupBoolBaggage debugKey ctx)
      then Just Debug
      else explicitSamplingState <$> lookupBoolBaggage samplingKey ctx
  in B3 (contextTraceID ctx) (contextSpanID ctx) (parentID $ _activeSpanReferences s) samplingState

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
  toJSON (B3 traceID spanID parentID state) =
    let
      defaultKVs = [traceIDHeader JSON..= traceID, spanIDHeader JSON..= spanID]
      parentKVs = (parentSpanIDHeader JSON..=) <$> maybeToList parentID
      sampledKVs = case state of
        Nothing -> []
        Just Debug -> [debugHeader JSON..= ("1" :: Text)]
        Just Accept -> [sampledHeader JSON..= ("1" :: Text)]
        Just Deny -> [sampledHeader JSON..= ("0" :: Text)]
    in JSON.object $ defaultKVs ++ parentKVs ++ sampledKVs

data ZipkinSpan = ZipkinSpan !(Maybe Endpoint) !Span

instance JSON.ToJSON ZipkinSpan where
  toJSON (ZipkinSpan mbEpt spn) =
    let
      ctx = spanContext spn
      requiredKVs =
        [ "traceId" JSON..= contextTraceID ctx
        , "name" JSON..= spanName spn
        , "id" JSON..= contextSpanID ctx
        , "timestamp" JSON..= microSeconds @Int64 (spanStartTime spn)
        , "duration" JSON..= microSeconds @Int64 (spanDuration spn)
        , "debug" JSON..= fromMaybe False (lookupBoolBaggage debugKey ctx)
        ]
      optionalKVs = catMaybes
        [ ("parentId" JSON..=) <$> parentID (_spanReferences spn)
        , ("localEndpoint" JSON..=) <$> mbEpt
        , ("remoteEndpoint" JSON..=) <$> lookupTag endpointKey spn
        , ("kind" JSON..=) <$> lookupTag kindKey spn
        ]
    in JSON.object $ requiredKVs ++ optionalKVs

microSeconds :: Integral a => NominalDiffTime -> a
microSeconds = round . (* 1000000)
