module Swerve.Server.Internal where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Network.HTTP.Types (hContentType)
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
import Swerve.API.Combinators (type (:>))
import Swerve.API.ContentTypes (class AllCTRender, class MimeUnrender, AcceptHeader(..), handleAcceptH, mimeUnrender)
import Swerve.API.Header (class ReadHeader, Header, readHeader)
import Swerve.API.Query (class ReadQuery, Query, readQuery)
import Swerve.API.ReqBody (ReqBody)
import Swerve.API.Resource (Resource)
import Swerve.API.StatusCode (class HasStatus, StatusP(..), toStatus)
import Swerve.API.Verb (class ReflectMethod, Verb, reflectMethod)
import Swerve.Server.Internal.HasConn (class HasConn)
import Swerve.Server.Internal.Method (methodCheck)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, QueryVar, Segment)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.RoutingApplication (RoutingApplication)
import Swerve.Server.InternalPathSub (class PathSub, PathEnd)
import Swerve.Utils (queryInfo)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class HasServer :: forall k. k -> Type -> Constraint
class HasServer api handler where 
  route :: Proxy api -> handler -> RoutingApplication Response

instance hasServerVerb :: 
  ( Parse path plist
  , PathSub plist api' 
  , ReflectMethod method 
  , HasConn api () conn
  , Server api' api (Verb method status path) handler conn conn
  ) => HasServer (Verb method status path :> api) handler where 
  route _ handler req resp = case validateMethod of 
    Left _  -> resp NotMatched
    Right _ -> server (Proxy :: _ api') (Proxy :: _ api) spec handler identity req resp
    where 
      api = Proxy :: _ api
      spec = Proxy :: _ (Verb method status path)
      validateMethod = methodCheck (reflectMethod (Proxy :: _ method)) req

class Server :: forall k1 k2. k1 -> k2 -> Type -> Type -> Row Type -> Row Type -> Constraint
class Server api spec verb handler (from :: Row Type) (to :: Row Type)| api spec handler -> from to where 
  server :: Proxy api -> Proxy spec -> Proxy verb -> handler -> Builder { | from } { | to } -> RoutingApplication Response

instance serverResource :: 
  ( AllCTRender ctype a 
  , HasStatus status
  ) => Server PathEnd (Resource a ctype) (Verb method status path) (Record to -> Aff a) to to where 
  server _ _ _ handler bldr req resp = do 
    let (conn :: Record to)  = Builder.build bldr $ unsafeCoerce {}  -- what can I do about this?
    resource <- handler conn
    case handleAcceptH (Proxy :: _ ctype) (AcceptHeader "*/*") resource of
      Nothing -> resp NotMatched
      Just (ct /\ body) -> do 
        resp $ Matched $ responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body

instance serverCapture :: 
  ( IsSymbol vname
  , ReadCapture t 
  , Row.Cons vname t () f
  , Row.Union f fy f' 
  , Row.Nub f' fy
  , Server api spec verb handler (capture :: Record fy | from ) (capture :: Record fy | to)
  ) => Server (CaptureVar vname :> api) (Capture vname t :> spec) verb handler (capture :: Record fy | from ) (capture :: Record fy | to ) where
  server _ _ verb handler bldr req resp = do 
    case Array.head $ pathInfo req of 
      Nothing  -> resp NotMatched
      Just var -> case readCapture var of 
        Nothing  -> resp NotMatched
        Just (val :: t) -> do 
          let rec = Record.insert (SProxy :: _ vname) val {}
              captureB = Builder.modify (SProxy :: _ "capture") (Record.merge rec)
          Console.logShow $ _.url $ unwrap (req' var 1)
          server api spec verb handler (captureB >>> bldr) (req' var 1) resp
    where 
      api = Proxy :: _ api
      spec = Proxy :: _ spec
      r = unwrap req 
      req' seg n = wrap $ r { url = String.drop ((String.length seg) + n)  r.url }

else instance serverCaptureFail :: 
  ( IsSymbol vname 
  , Symbol.Append "could not be found for path attribute :" vname errMsg
  , TE.Fail (Above (Quote (Capture vname a)) (Text errMsg))
  ) => Server (CaptureVar vname :> api) spec verb handler from to where 
  server api spec ver handler bldr req resp = unsafeCrashWith ""

instance serverQuery :: 
  ( IsSymbol vname
  , ReadQuery t 
  , Row.Cons vname t () f
  , Row.Union f fy f' 
  , Row.Nub f' fy
  , Server api spec verb handler (query :: Record fy | from ) (query :: Record fy | to)
  ) => Server (QueryVar vname :> api) (Query vname t :> spec) verb handler (query :: Record fy | from ) (query :: Record fy | to ) where
  server _ _ verb handler bldr req resp = do 
    case Map.lookup queryParam queries of 
      Nothing  -> resp NotMatched
      Just var -> case readQuery var of 
        Nothing  -> resp NotMatched
        Just (val :: t) -> do 
          let rec = Record.insert (SProxy :: _ vname) val {}
              queryB = Builder.modify (SProxy :: _ "query") (Record.merge rec)
          server api spec verb handler (queryB >>> bldr) req' resp
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
  ) => Server (QueryVar vname :> api) spec verb handler from to where 
  server api spec ver handler bldr req resp = unsafeCrashWith ""

instance serverHeader :: 
  ( IsSymbol vname
  , ReadHeader t 
  , Row.Cons vname t () f
  , Row.Union f fy f' 
  , Row.Nub f' fy
  , Server PathEnd spec verb handler (header :: Record fy | from ) (header :: Record fy | to)
  ) => Server PathEnd (Header vname t :> spec) verb handler (header :: Record fy | from ) (header :: Record fy | to ) where
  server api _ verb handler bldr req resp = do 
    case Map.lookup (wrap headerParam) headers of 
      Nothing  -> resp NotMatched
      Just var -> case readHeader var of 
        Nothing  -> resp NotMatched
        Just (val :: t) -> do 
          let rec = Record.insert (SProxy :: _ vname) val {}
              headerB = Builder.modify (SProxy :: _ "header") (Record.merge rec)
          server api spec verb handler (headerB >>> bldr) req resp
    where 
      headers = Map.fromFoldable $ _.headers $ unwrap req
      headerParam = reflectSymbol (SProxy :: _ vname)
      spec = Proxy :: _ spec

instance serverReqBody :: 
  ( MimeUnrender ctypes a 
  , Server PathEnd spec verb handler (body :: a | from ) (body :: a | to)
  ) => Server PathEnd (ReqBody a ctypes :> spec) verb handler (body :: a | from ) (body :: a | to ) where
  server api _ verb handler bldr req@(Request { body: Nothing }) resp = resp NotMatched
  server api _ verb handler bldr req@(Request { body: Just stream }) resp = do 
    bodyStr <- Aff.makeAff \done -> do
      bufs <- Ref.new []
      Stream.onData stream \buf ->
        void $ Ref.modify (_ <> [buf]) bufs
      Stream.onEnd stream do
        body <- Ref.read bufs >>= Buffer.concat >>= Buffer.toString UTF8
        done $ Right body
      pure Aff.nonCanceler

    case mimeUnrender (Proxy :: _ ctypes) bodyStr of 
      Left e     -> resp NotMatched
      Right body -> do
          let bodyB = Builder.modify (SProxy :: _ "body") (const body)
          server api spec verb handler (bodyB >>> bldr) req resp
    where 
      spec = Proxy :: _ spec

instance serverSegment :: 
  ( IsSymbol seg
  , Server api spec verb handler from to
  ) => Server (Segment seg :> api) spec verb handler from to where 
  server _ spec verb handler bldr req resp = case pathInfo req of 
    []   | seg == "/" -> server api spec verb handler bldr (req' 0) resp 
    _    | seg == "/" -> server api spec verb handler bldr (req' 0) resp 
    path | Just s <- Array.head path
         , s == seg -> server api spec verb handler bldr (req' 1) resp 
    otherwise ->  resp NotMatched
    where 
      
      api = Proxy :: _ api
      seg = reflectSymbol (SProxy :: _ seg)
      r = unwrap req
      req' n = wrap $ r { url = String.drop ((String.length seg) + n)  r.url }