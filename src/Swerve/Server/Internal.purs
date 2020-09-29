module Swerve.Server.Internal where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Ref as Ref
import Heterogeneous.Folding (class HFoldlWithIndex)
import Network.HTTP.Types (hAccept, hContentType)
import Network.Wai (Request(..), Response, pathInfo, responseStr)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream as Stream
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Prim.TypeError (Above, Quote, Text)
import Prim.TypeError as TE
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Swerve.API.Capture (class ReadCapture, Capture, readCapture)
import Swerve.API.Combinators (type (:>), type (:<|>), (:<|>))
import Swerve.API.ContentTypes (class AllCTRender, class MimeUnrender, AcceptHeader(..), handleAcceptH, mimeUnrender)
import Swerve.API.Guard (Guard)
import Swerve.API.Header (class ReadHeader, Header, Headers(..), readHeader)
import Swerve.API.Query (class ReadQuery, Query, readQuery)
import Swerve.API.Raw (Raw')
import Swerve.API.ReqBody (ReqBody)
import Swerve.API.Resource (Resource)
import Swerve.API.StatusCode (class HasStatus, StatusP(..), toStatus)
import Swerve.API.Verb (class ReflectMethod, Verb, reflectMethod)
import Swerve.Internal.Router (Router, runRouter)
import Swerve.Server.Internal.HasConn (class HasConn)
import Swerve.Server.Internal.Method (methodCheck)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, QueryVar, Segment)
import Swerve.Server.Internal.RoutingApplication (RouteResult(..), RoutingApplication)
import Swerve.Server.InternalPathSub (class PathSub, PathEnd)
import Swerve.Utils (HeadersUnfold, headersToUnfoldable, queryInfo)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class HasServer :: forall k. k -> Row Type -> Type -> Constraint
class HasServer api context handler | api context -> handler where 
  route :: Proxy api -> Record context -> handler -> RoutingApplication Response

instance hasServerAlt ::
  ( HasServer apia context handlera 
  , HasServer apib context handlerb 
  ) => HasServer (apia :<|> apib) context (handlera :<|> handlerb) where 
  route _ ctx (handlera :<|> handlerb) req resp = 
    resp $ Choice 
              (\req resp -> route (Proxy :: _ apia) ctx handlera req resp) 
              (\req resp  -> route (Proxy :: _ apib) ctx handlerb req resp)

instance hasServerRaw ::
  ( IsSymbol path
  , Server PathEnd api (Raw' path) context handler () ()
  ) => HasServer (Raw' path :> api) context handler where 
  route _ ctx handler req@(Request {url}) resp 
    | url == (reflectSymbol (SProxy :: _ path)) = do 
      eroute <- runRouter $ server (Proxy :: _ PathEnd) (Proxy :: _ api) (Proxy :: _ (Raw' path)) ctx handler identity req 
      resp $ either (const $ NotMatched) identity eroute 
    | otherwise = resp NotMatched

instance hasServerGuard :: 
  ( IsSymbol gname
  , Row.Cons gname (Request -> Aff (Either String t)) f ctx
  , HasServer api ctx handler 
  ) => HasServer (Guard gname t :> api) ctx (t -> handler) where 
  route _ ctx handler req resp = do 
    guardCtx <- guardFn req 
    case guardCtx of 
      Left e    -> resp NotMatched
      Right gctx -> route (Proxy :: _ api) ctx (handler gctx) req resp
    where 
      guardFn = Record.get (SProxy :: _ gname) ctx

instance hasServerVerb :: 
  ( Parse path plist
  , PathSub plist api' 
  , ReflectMethod method 
  , HasConn api () conn
  , Server api' api (Verb method status path) context handler conn conn
  ) => HasServer (Verb method status path :> api) context handler where 
  route _ ctx handler req resp = case validateMethod of 
    Left _  -> resp NotMatched
    Right _ -> do 
      eroute <- runRouter $ server (Proxy :: _ api') (Proxy :: _ api) spec ctx handler identity req
      resp $ either (const $ NotMatched) identity eroute 
    where 
      api = Proxy :: _ api
      spec = Proxy :: _ (Verb method status path)
      validateMethod = methodCheck (reflectMethod (Proxy :: _ method)) req

class Server :: forall k1 k2. k1 -> k2 -> Type -> Row Type -> Type -> Row Type -> Row Type -> Constraint
class Server api spec verb context handler (from :: Row Type) (to :: Row Type)| api spec handler -> from to where 
  server :: Proxy api -> Proxy spec -> Proxy verb -> Record context -> handler -> Builder { | from } { | to } -> Request -> Router (RouteResult Response)

instance serverRaw' ::
  (IsSymbol path) => Server PathEnd (Resource Unit Unit) (Raw' path) context (Request -> (Response -> Aff Unit) -> Aff Unit) to to where 
  server api _ _ _ handler conn req@(Request {url})  
    | url == (reflectSymbol (SProxy :: _ path)) = pure $ RawRoute \req' resp -> handler req' resp 
    | otherwise = pure NotMatched

instance serverResourceHeader :: 
  ( AllCTRender ctype a 
  , HasStatus status
  , HFoldlWithIndex HeadersUnfold Unit { | hdrs } (Array (Tuple CaseInsensitiveString String))
  ) => Server PathEnd (Resource (Headers hdrs a) ctype) (Verb method status path) ctx (Record to -> Aff (Headers hdrs a)) to to where 
  server _ _ _ _ handler bldr req = do 
    let (conn :: Record to)  = Builder.build bldr $ unsafeCoerce {}  -- what can I do about this?
    (Headers headers resource)  <- liftAff $ handler conn
    let hdrs = headersToUnfoldable headers
    case handleAcceptH (Proxy :: _ ctype) (getAcceptHeader req) resource of
      Nothing -> pure $ NotMatched
      Just (ct /\ body) -> do 
        pure $ Matched $ responseStr (toStatus (StatusP :: _ status)) (hdrs <> [hContentType /\ ct]) body

else instance serverResource :: 
  ( AllCTRender ctype a 
  , HasStatus status
  ) => Server PathEnd (Resource a ctype) (Verb method status path) ctx (Record to -> Aff a) to to where 
  server _ _ _ _ handler bldr req = do 
    let (conn :: Record to)  = Builder.build bldr $ unsafeCoerce {}  -- what can I do about this?
    resource <- liftAff $ handler conn
    case handleAcceptH (Proxy :: _ ctype) (getAcceptHeader req) resource of
      Nothing -> pure NotMatched
      Just (ct /\ body) -> do 
        pure $ Matched $ responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body

instance serverCapture :: 
  ( IsSymbol vname
  , ReadCapture t 
  , Row.Cons vname t () f
  , Row.Union f fy f' 
  , Row.Nub f' fy
  , Server api spec verb ctx handler (capture :: Record fy | from ) (capture :: Record fy | to)
  ) => Server (CaptureVar vname :> api) (Capture vname t :> spec) verb ctx handler (capture :: Record fy | from ) (capture :: Record fy | to ) where
  server _ _ verb ctx handler bldr req = do 
    case Array.head $ pathInfo req of 
      Nothing  -> pure NotMatched
      Just var -> case readCapture var of 
        Nothing  -> pure NotMatched
        Just (val :: t) -> do 
          let rec = Record.insert (SProxy :: _ vname) val {}
              captureB = Builder.modify (SProxy :: _ "capture") (Record.merge rec)
          server api spec verb ctx handler (captureB >>> bldr) (req' var 1)
    where 
      api = Proxy :: _ api
      spec = Proxy :: _ spec
      r = unwrap req 
      req' seg n = wrap $ r { url = String.drop ((String.length seg) + n)  r.url }

else instance serverCaptureFail :: 
  ( IsSymbol vname 
  , Symbol.Append "could not be found for path attribute :" vname errMsg
  , TE.Fail (Above (Quote (Capture vname a)) (Text errMsg))
  ) => Server (CaptureVar vname :> api) spec verb ctx handler from to where 
  server _ _ _ _ _ _ _ = unsafeCrashWith ""

instance serverQuery :: 
  ( IsSymbol vname
  , ReadQuery t 
  , Row.Cons vname t () f
  , Row.Union f fy f' 
  , Row.Nub f' fy
  , Server api spec verb ctx handler (query :: Record fy | from ) (query :: Record fy | to)
  ) => Server (QueryVar vname :> api) (Query vname t :> spec) verb ctx handler (query :: Record fy | from ) (query :: Record fy | to ) where
  server _ _ verb ctx handler bldr req = do 
    case Map.lookup queryParam queries of 
      Nothing  -> pure NotMatched
      Just var -> case readQuery var of 
        Nothing  -> pure NotMatched
        Just (val :: t) -> do 
          let rec = Record.insert (SProxy :: _ vname) val {}
              queryB = Builder.modify (SProxy :: _ "query") (Record.merge rec)
          server api spec verb ctx handler (queryB >>> bldr) req'
    where 
      queries = Map.fromFoldable $ queryInfo $ _.url $ unwrap req
      queryParam = reflectSymbol (SProxy :: _ vname)
      api = Proxy :: _ api
      spec = Proxy :: _ spec
      url' = String.joinWith "&" $ map (\t -> fst t <> "=" <> snd t) $ Map.toUnfoldable $ Map.delete queryParam queries
      r = unwrap req 
      req' = wrap $ r { url = url' }

else instance serverQueryFail :: 
  ( IsSymbol vname 
  , Symbol.Append "could not be found for path query :" vname errMsg
  , TE.Fail (Above (Quote (Query vname a)) (Text errMsg))
  ) => Server (QueryVar vname :> api) spec verb ctx handler from to where 
  server _ _ _ _ _ _ _ = unsafeCrashWith ""

instance serverHeader :: 
  ( IsSymbol vname
  , ReadHeader t 
  , Row.Cons vname t () f
  , Row.Union f fy f' 
  , Row.Nub f' fy
  , Server PathEnd spec verb ctx handler (header :: Record fy | from ) (header :: Record fy | to)
  ) => Server PathEnd (Header vname t :> spec) verb ctx handler (header :: Record fy | from ) (header :: Record fy | to ) where
  server api _ verb ctx handler bldr req = do 
    case Map.lookup (wrap headerParam) headers of 
      Nothing  -> pure NotMatched
      Just var -> case readHeader var of 
        Nothing  -> pure NotMatched
        Just (val :: t) -> do 
          let rec = Record.insert (SProxy :: _ vname) val {}
              headerB = Builder.modify (SProxy :: _ "header") (Record.merge rec)
          server api spec verb ctx handler (headerB >>> bldr) req
    where 
      headers = Map.fromFoldable $ _.headers $ unwrap req
      headerParam = reflectSymbol (SProxy :: _ vname)
      spec = Proxy :: _ spec

instance serverReqBody :: 
  ( MimeUnrender ctypes a 
  , Server PathEnd spec verb ctx handler (body :: a | from ) (body :: a | to)
  ) => Server PathEnd (ReqBody a ctypes :> spec) verb ctx handler (body :: a | from ) (body :: a | to ) where
  server _ _ _ _ _ _ req@(Request { body: Nothing }) = pure NotMatched
  server api _ verb ctx handler bldr req@(Request { body: Just stream }) = do 
    bodyStr <- liftAff $ Aff.makeAff \done -> do
      bufs <- Ref.new []
      Stream.onData stream \buf ->
        void $ Ref.modify (_ <> [buf]) bufs
      Stream.onEnd stream do
        body <- Ref.read bufs >>= Buffer.concat >>= Buffer.toString UTF8
        done $ Right body
      pure Aff.nonCanceler

    case mimeUnrender (Proxy :: _ ctypes) bodyStr of 
      Left e     -> pure NotMatched
      Right body -> do
          let bodyB = Builder.modify (SProxy :: _ "body") (const body)
          server api spec verb ctx handler (bodyB >>> bldr) req
    where 
      spec = Proxy :: _ spec

instance serverSegment :: 
  ( IsSymbol seg
  , Server api spec verb ctx handler from to
  ) => Server (Segment seg :> api) spec verb ctx handler from to where 
  server _ spec verb ctx handler bldr req = case pathInfo req of 
    []   | seg == "/" -> server api spec verb ctx handler bldr (req' 0)
    _    | seg == "/" -> server api spec verb ctx handler bldr (req' 0) 
    path | Just s <- Array.head path
         , s == seg -> server api spec verb ctx handler bldr (req' 1)
    otherwise ->  pure NotMatched
    where 
      api = Proxy :: _ api
      seg = reflectSymbol (SProxy :: _ seg)
      r = unwrap req
      req' n = wrap $ r { url = String.drop ((String.length seg) + n)  r.url }


getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader = AcceptHeader <<< fromMaybe ct_wildcard <<< Map.lookup hAccept <<< Map.fromFoldable <<< _.headers <<< unwrap

ct_wildcard :: String
ct_wildcard = "*" <> "/" <> "*"