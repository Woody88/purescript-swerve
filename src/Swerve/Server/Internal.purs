module Swerve.Server.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Lens (to)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Network.HTTP.Types (hContentType)
import Network.Wai (Response, pathInfo, responseStr)
import Prim.Row as Row
import Record.Builder (Builder)
import Record.Builder as Builder
import Swerve.API.Capture (class ReadCapture, Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.ContentTypes (class AllCTRender, AcceptHeader(..), handleAcceptH)
import Swerve.API.Resource (Resource)
import Swerve.API.StatusCode (class HasStatus, StatusP(..), toStatus)
import Swerve.API.Verb (class ReflectMethod, Verb, reflectMethod)
import Swerve.Server.Internal.Method (methodCheck)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, PCons, PList, PNil, Segment)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.RoutingApplication (RoutingApplication)
import Type.Proxy (Proxy(..))

class HasServer :: forall k. k -> Type -> Constraint
class HasServer api handler where 
  route :: Proxy api -> handler -> RoutingApplication Response

instance hasServerVerb :: 
  ( Parse path plist
  , PathToSub plist api api' 
  , ReflectMethod method 
  , Server api' (Verb method status path) handler (Record ()) (Record conn)
  ) => HasServer (Verb method status path :> api) handler where 
  route _ handler req resp = case validateMethod of 
    Left _  -> Console.log "in" *> resp NotMatched
    Right _ -> server (Proxy :: _ api') spec handler (flip Builder.build {})  req resp
    where 
      spec = Proxy :: _ (Verb method status path)
      validateMethod = methodCheck (reflectMethod (Proxy :: _ method)) req

class Server :: forall k1 k2. k1 -> k2 -> Type -> Type -> Type -> Constraint
class Server api spec handler from to | api spec handler -> from to where 
  server :: Proxy api -> Proxy spec -> handler -> (Builder from  to -> to ) -> RoutingApplication Response

instance serverResource :: 
  ( AllCTRender ctype a 
  , HasStatus status
  ) => Server (Resource a ctype) (Verb method status path) (Aff a) from to where 
  server _ _ handler bldr req resp = do 
    resource <- handler
    case handleAcceptH (Proxy :: _ ctype) (AcceptHeader "*/*") resource of
      Nothing -> Console.log "in" *> resp NotMatched
      Just (ct /\ body) -> do 
        resp $ Matched $ responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body


instance serverSegment :: 
  ( IsSymbol seg
  , Server api spec handler from to 
  ) => Server (Segment seg :> api) spec handler from to where 
  server _ spec handler bldr req resp = case pathInfo req of 
    []   | seg == "/" -> server api spec handler bldr (req' 0) resp 
    _    | seg == "/" -> server api spec handler bldr (req' 0) resp 
    path | (String.joinWith "/" path) == seg -> server api spec handler bldr (req' 1) resp 
    otherwise -> Console.log seg *>  resp NotMatched
    where 
      
      api = Proxy :: _ api
      seg = reflectSymbol (SProxy :: _ seg)
      r = unwrap req
      req' n = wrap $ r { url = String.drop ((String.length seg) + n)  r.url }


class MkConn :: forall k. k -> Row Type -> Row Type -> Constraint
class MkConn api from to | api -> from to where
  mkConn :: Proxy api -> Builder { | from } { | to }

instance mkConnResource :: MkConn (Resource a ctype) to to where 
  mkConn _ = identity

instance mkConnCapture :: 
  ( Row.Lacks "capture" from
  , Row.Cons "capture" (Builder (Record ()) (Record ())) from (capture :: (Builder (Record ()) (Record ())) | from)
  , MkConn api (capture :: (Builder (Record ()) (Record ())) | from) to
  ) => MkConn (Capture sym t :> api) from to where 
  mkConn _ = mkConn (Proxy :: _ api) <<< Builder.insert (SProxy :: _ "capture") identity


toConn :: forall api to. MkConn api () to => Proxy api -> Builder (Record ()) { | to }
toConn = mkConn 

    -- let (empty :: Builder (Record ()) (Record ())) = identity
    -- Builder.insert (SProxy :: _ "capture") 1 <<< mkConn (Proxy :: _ api)

class PathToSub :: forall k api. PList -> api -> k -> Constraint
class PathToSub (plist :: PList) api k | plist api -> k 

instance pathToSubNil :: PathToSub PNil api api 

instance pathToSubCapture :: PathToSub rest api b => PathToSub (PCons (CaptureVar sym) rest) api (CaptureVar sym :> b)

instance pathToSubSegment' :: PathToSub rest api b => PathToSub (PCons (Segment "") rest) api (Segment "/" :> b)
else instance pathToSubSegment :: PathToSub rest api b => PathToSub (PCons (Segment sym) rest) api (Segment sym :> b)

-- class ParseUrl (plist :: PList) api conn where 
--   parseUrl :: Proxy plist -> Proxy api -> Either String { | conn }

-- class ParseCapture (plist :: PList) api (from :: Row Type) (to :: Row Type) where
--   parseCapture :: Proxy plist -> Proxy api -> Either String (Builder { | from}  { | to})

-- instance parseUrlNil :: ParseUrl PNil api to to where 
--   parseUrl _ _ = pure identity

-- instance parseUrlCapture :: 
--   ( IsSymbol val 
--   , IsSymbol vname 
--   , Row.Lacks vname from 
--   , Row.Cons vname t from from' 
--   , ReadCapture t 
--   ) => ParseUrl (PCons (CaptureVar val) rest) (Capture vname t :> api) from to where 
--   parseUrl _ _ = do

--     where 
--       name = SProxy :: _ val
--       value = reflectSymbol $ SProxy :: _ val
--       t = Proxy :: _ t 
--       api = Proxy :: _ api 







-- import Prelude

-- import Control.Monad.Except (class MonadError, runExceptT)
-- import Data.Either (Either(..))
-- import Effect.Aff (Aff)
-- import Network.HTTP.Types (notFound404)
-- import Network.Wai (Application, responseStr)
-- import Swerve.Server.Internal.Response (SwerveResponse(..))
-- import Swerve.Server.Internal.RouterI (class RouterI, routerI)
-- import Swerve.Server.Internal.ServerError (ServerError(..), responseServerError)
-- import Type.Proxy (Proxy)

-- class HasServer api handler m where 
--   route :: Proxy api -> (forall a. m a -> Aff (Either ServerError a)) -> handler -> Application 

-- instance hasServerAlt :: 
--   ( RouterI api handler m
--   , Monad m
--   , MonadError ServerError m
--   ) => HasServer api handler m where 
--   route api runM handler req resp = do 
--     routerResult <- runExceptT $ routerI api handler req
--     case routerResult of 
--       Left (ServerError e) -> resp $ responseStr notFound404 [] e.errMessage
--       Right handlerM -> do 
--         result <- runM handlerM
--         case result of
--           Left e   -> resp $ responseServerError e
--           Right rsp -> case rsp of 
--             Rsp response -> resp response
--             Raw app      -> app req resp