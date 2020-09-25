module Swerve.Server.Internal where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Network.HTTP.Types (hContentType)
import Network.Wai (Response, pathInfo, responseStr)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Prim.TypeError (Text)
import Prim.TypeError as TE
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Swerve.API.Capture (class ReadCapture, Capture, readCapture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.ContentTypes (class AllCTRender, AcceptHeader(..), handleAcceptH)
import Swerve.API.Resource (Resource)
import Swerve.API.StatusCode (class HasStatus, StatusP(..), toStatus)
import Swerve.API.Verb (class ReflectMethod, Verb, reflectMethod)
import Swerve.Server.Internal.HasConn (class HasConn)
import Swerve.Server.Internal.Method (methodCheck)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, Segment)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.RoutingApplication (RoutingApplication)
import Swerve.Server.InternalPathSub (class PathSub)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class HasServer :: forall k. k -> Type -> Constraint
class HasServer api handler where 
  route :: Proxy api -> handler -> RoutingApplication Response

instance hasServerVerb :: 
  ( Parse path plist
  , PathSub plist api api' 
  , ReflectMethod method 
  , HasConn api to
  , Server api' api' (Verb method status path) handler to to
  ) => HasServer (Verb method status path :> api) handler where 
  route _ handler req resp = case validateMethod of 
    Left _  -> resp NotMatched
    Right _ -> server (Proxy :: _ api') (Proxy :: _ api') spec handler identity req resp
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
  ) => Server (Resource a ctype) spec (Verb method status path) (Record to -> Aff a) to to where 
  server _ _ _ handler bldr req resp = do 
    let (conn :: Record to)  = Builder.build bldr $ unsafeCoerce {}  -- what can I do about this?
    resource <- handler conn
    case handleAcceptH (Proxy :: _ ctype) (AcceptHeader "*/*") resource of
      Nothing -> resp NotMatched
      Just (ct /\ body) -> do 
        resp $ Matched $ responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body

instance serverCapture' :: 
  ( IsSymbol vname
  , ReadCapture t 
  , Row.Cons vname t () f
  , Row.Union f fy f' 
  , Row.Nub f' fy
  , Server api spec verb handler (capture :: Record fy | from ) (capture :: Record fy | to)
  )=> Server (Capture vname t :> api) (CaptureVar vname :> spec) verb handler (capture :: Record fy | from ) (capture :: Record fy | to ) where
  server _ _ verb handler bldr req resp = do 
    case Array.head $ pathInfo req of 
      Nothing  -> resp NotMatched
      Just var -> case readCapture var of 
        Nothing  -> resp NotMatched
        Just (val :: t) -> do 
          let rec = Record.insert (SProxy :: _ vname) val {}
              captureB = Builder.modify (SProxy :: _ "capture") (Record.merge rec)
          server api spec verb handler (captureB >>> bldr) (req' var 1) resp
    where 
      api = Proxy :: _ api
      spec = Proxy :: _ spec
      r = unwrap req 
      req' seg n = wrap $ r { url = String.drop ((String.length seg) + n)  r.url }

else instance serverCapture2 :: 
  ( IsSymbol vname 
  , Symbol.Append "Route does not define a capture :" vname errMsg 
  , TE.Fail (Text errMsg) 
  ) => Server (Capture vname t :> api) spec verb handler from to where 
  server api spec ver handler bldr req resp = unsafeCrashWith ""

instance serverCapture :: 
  ( Server api spec verb handler from to 
  ) => Server (CaptureVar vname :> api) spec verb handler from to where 
  server _ spec verb handler bldr req resp = server (Proxy :: _ api) spec verb handler bldr req resp 

instance serverSegment :: 
  ( IsSymbol seg
  , Server api spec verb handler from to
  ) => Server (Segment seg :> api) (Segment seg :> spec) verb handler from to where 
  server _ _ verb handler bldr req resp = case pathInfo req of 
    []   | seg == "/" -> server api spec verb handler bldr (req' 0) resp 
    _    | seg == "/" -> server api spec verb handler bldr (req' 0) resp 
    path | Just s <- Array.head path
         , s == seg -> server api spec verb handler bldr (req' 1) resp 
    otherwise ->  resp NotMatched
    where 
      
      api = Proxy :: _ api
      spec = Proxy :: _ spec
      seg = reflectSymbol (SProxy :: _ seg)
      r = unwrap req
      req' n = wrap $ r { url = String.drop ((String.length seg) + n)  r.url }