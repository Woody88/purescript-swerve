module Swerve.Server.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Ref as Ref
import Network.HTTP.Types (Method, hAccept, hContentType)
import Network.HTTP.Types.Method (methodGet, methodHead)
import Network.Wai (Request(..), Application, responseStr)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream as Stream
import Record as Record
import Swerve.API.BasicAuth (BasicAuth)
import Swerve.API.Capture (class ReadCapture, readCapture)
import Swerve.API.ContentType (class AllCTUnrender, class AllMime, AcceptHeader(..), canHandleAcceptH, canHandleCTypeH)
import Swerve.API.Header (class ReadHeader, readHeader)
import Swerve.API.Method (class ToMethod, toMethod)
import Swerve.API.QueryParam (class ReadQuery, readQuery)
import Swerve.API.Status (class HasStatus)
import Swerve.API.Types (type (:<|>), type (:>), Capture, Header, QueryParam, Raise, Raw, ReqBody, Respond', Verb, (:<|>))
import Swerve.Server.Internal.BasicAuth (BasicAuthCheck, runBasicAuth)
import Swerve.Server.Internal.Delayed (Delayed, addAuthCheck, addAcceptCheck, addBodyCheck, addCapture, addHeaderCheck, addMethodCheck, addParameterCheck, runAction, runDelayed)
import Swerve.Server.Internal.DelayedIO (DelayedIO, delayedFail, delayedFailFatal, withRequest)
import Swerve.Server.Internal.EvalServer (class EvalServer, toHandler, toHoistServer)
import Swerve.Server.Internal.Response (Response(..))
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.Router (Router, Router'(..), choice, leafRouter, pathRouter)
import Swerve.Server.Internal.ServerError (err400, err405, err406, err415, err500)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Server' (api :: Type)  (m :: Type -> Type) 

type Server spec = Server' spec Aff

class HasServer :: Type -> Row Type -> (Type -> Type) -> Type -> Constraint
class HasServer api context m handler | api -> handler context where 
  route :: forall env. Proxy api -> Proxy m -> Record context -> Delayed env (Server api) -> Router env
  hoistServerWithContext :: forall n. Proxy api -> Proxy context -> (forall x. m x -> n x) -> Server' api m -> Server' api n

instance hasServerAlt :: 
  ( EvalServer (Server' (a :<|> b) m) ((Server' a m) :<|> (Server' b m))
  , HasServer a context m handlera
  , HasServer b context m handlerb
  ) => HasServer (a :<|> b) context m (handlera :<|> handlerb) where 
  hoistServerWithContext _ pc nt server = let 
    (servera :<|> serverb) = (toHoistServer server)
    in 
      unsafeCoerce (hoistServerWithContext (Proxy :: _ a) pc nt servera :<|> hoistServerWithContext (Proxy :: _ b) pc nt serverb)

  route _ m context server = choice (route pa m context ((\ (a :<|> _) -> a) <$> toHandler server))
                                    (route pb m context ((\ (_ :<|> b) -> b) <$> toHandler server))
    where
      pa = Proxy :: _ a 
      pb = Proxy :: _ b 

instance hasServerRaw :: 
  ( TypeEquals Application application
  , EvalServer (Server' Raw m) (m application)
  ) => HasServer Raw context m (m application) where 
  hoistServerWithContext _ _ f m  = unsafeCoerce $ f (toHoistServer m)
  route _ _ _ rawApplication = RawRouter $ \env request respond -> do   
    r <- runDelayed (toHandler rawApplication) env request
    case r of 
      Route appM -> do 
        app <- appM
        app request (respond <<< Route)
      Fail a      -> respond $ Fail a 
      FailFatal e -> respond $ FailFatal e

instance hasServerVerb :: 
  ( HasStatus status
  , ToMethod method
  , AllMime ctypes
  , EvalServer (Server' (Verb method a status hdrs ctypes) m) (m (Response rs a))
  ) => HasServer (Verb method a status hdrs ctypes) context m (m (Response (Either (Respond' status hdrs) l) a)) where 
  hoistServerWithContext _ _ nt s = unsafeCoerce (nt $ toHoistServer s)
  route _ _ _ subserver = leafRouter route'
    where 
      status = Proxy :: _ status 
      method = toMethod (Proxy :: _ method)
      ctypesP = Proxy :: _ ctypes 
      route' env request respond = do 
        let 
          accH   = getAcceptHeader request
          action = toHandler subserver `addMethodCheck` methodCheck method request
                                       `addAcceptCheck` acceptCheck ctypesP accH
        runAction action env request respond 
          \(Response v) -> case v of 
              Right s -> Route $ responseStr s.status [] s.content
              Left f  -> Route $ responseStr f.status [] f.content

instance hasServerRaise :: 
  ( HasStatus status
  , EvalServer (Server' ((Raise status hdrs ctypes) :> api) m) (Server' api m)
  , HasServer api context m (m (Response handler a)) 
  ) => HasServer ((Raise status hdrs ctypes) :> api) context m (m (Response (Either (Respond' status hdrs) handler) a)) where 
  hoistServerWithContext _ pc nt s = unsafeCoerce $ hoistServerWithContext (Proxy :: Proxy api) pc nt (toHoistServer s)
  route _ m ctx subserver = route (Proxy :: _ api) m ctx (toHandler subserver) 

instance hasServerReqBody :: 
  ( AllCTUnrender ctypes a
  , EvalServer (Server' (ReqBody a ctypes :> api) m) (a -> Server' api m)
  , HasServer api context m handler 
  ) => HasServer (ReqBody a ctypes :> api) context m (a -> handler) where 
  hoistServerWithContext _ pc nt s = unsafeCoerce $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (toHoistServer s)
  route _ m context subserver = route (Proxy :: Proxy api) m context $ addBodyCheck (toHandler subserver) ctCheck bodyCheck
    where 
      ctCheck = withRequest $ \(Request req) -> do
        let hdrs = Map.fromFoldable req.headers 
            contentTypeH = fromMaybe "application/octet-stream" $ Map.lookup hContentType hdrs
        case canHandleCTypeH (Proxy :: Proxy ctypes) contentTypeH :: Maybe (String -> Either String a) of
          Nothing -> delayedFail err415
          Just f  -> pure f

      bodyCheck f = withRequest $ \request -> do
        mrqbody <- f <$> liftAff (requestBody request)

        case mrqbody of
          Left e  -> delayedFailFatal $ err500 { content = e }
          Right v -> pure v

      requestBody (Request req) = Aff.makeAff \done -> do
        case req.body of 
          Nothing     -> done $ Right "" 
          Just stream -> do 
            bufs <- Ref.new []

            Stream.onData stream \buf ->
              void $ Ref.modify (_ <> [buf]) bufs

            Stream.onEnd stream do
              body <- Ref.read bufs >>= Buffer.concat >>= Buffer.toString UTF8
              done $ Right body
        pure Aff.nonCanceler

instance hasServerCapture :: 
  ( ReadCapture a
  , EvalServer (Server' (Capture a :> api) m) (a -> Server' api m)
  , HasServer api context m handler
  ) => HasServer (Capture a :> api) context m (a -> handler) where 
  hoistServerWithContext _ pc nt s = unsafeCoerce $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (toHoistServer s)
  route _ m ctx subserver =
    CaptureRouter $ 
      route (Proxy :: _ api) m ctx
        (addCapture (toHandler subserver) \c -> withRequest \request -> 
          case readCapture c of 
            Nothing           -> delayedFail err400 
            Just (piece :: a) -> pure piece)

instance hasServerHeader :: 
  ( IsSymbol sym
  , ReadHeader a
  , EvalServer (Server' (Header sym a :> api) m) (a -> Server' api m)
  , HasServer api context m handler
  ) => HasServer (Header sym a :> api) context m (a -> handler) where 
  hoistServerWithContext _ pc nt s = unsafeCoerce $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (toHoistServer s)
  route _ m ctx subserver = route (Proxy :: _ api) m ctx delayed
    where 
      delayed = addHeaderCheck (toHandler subserver) <<< withRequest $ 
        \(Request req) -> let 
          key = reflectSymbol (SProxy :: _ sym)
          hMap = Map.fromFoldable $ req.headers 
          in case Map.lookup (wrap key) hMap >>= readHeader of 
            Nothing -> delayedFail err400 { content = req.url }
            Just (piece :: a) -> pure piece

instance hasServerQuery :: 
  ( IsSymbol sym
  , ReadQuery a
  , EvalServer (Server' (QueryParam sym a :> api) m) (Maybe a -> Server' api m)
  , HasServer api context m handler
  ) => HasServer (QueryParam sym a :> api) context m (Maybe a -> handler) where 
  hoistServerWithContext _ pc nt s = unsafeCoerce $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (toHoistServer s)
  route _ m ctx subserver = route (Proxy :: _ api) m ctx delayed
    where 
      delayed = addParameterCheck (toHandler subserver) <<< withRequest $ 
        \(Request req) -> let 
          key = reflectSymbol (SProxy :: _ sym)
          qMap = Map.fromFoldable $ req.queryString
          in case Map.lookup key qMap >>= (\mParam -> mParam >>= readQuery) of 
            Nothing -> delayedFail err400 { content = req.url }
            Just (piece :: Maybe a) -> pure piece

instance hasServerSegment :: 
  ( IsSymbol path
  , EvalServer (Server' (path :> api) m) (Server' api m)
  , HasServer api context m handler
  ) => HasServer (path :> api) context m handler where 
  hoistServerWithContext _ pc nt s = unsafeCoerce $ hoistServerWithContext (Proxy :: _ api) pc nt (toHoistServer s)
  route _ m ctx subserver = pathRouter path (route (Proxy :: _ api) m ctx (toHandler subserver))
    where 
      path = reflectSymbol (SProxy :: _ path)

instance hasServerBasicAuth ::
  ( IsSymbol realm 
  , EvalServer (Server' (BasicAuth realm usr :> api) m) (usr -> (Server' api m))
  , HasServer api (basicAuth :: BasicAuthCheck usr | context) m handler
  ) => HasServer (BasicAuth realm usr :> api) (basicAuth :: BasicAuthCheck usr | context) m (usr -> handler) where
  hoistServerWithContext _ pc nt s = unsafeCoerce $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (toHoistServer s)
  route _ m ctx subserver = route (Proxy :: Proxy api) m ctx ((toHandler subserver) `addAuthCheck` authCheck)
    where
       realm = reflectSymbol (SProxy :: _ realm)
       basicAuthContext = Record.get (SProxy :: _ "basicAuth") ctx
       authCheck = withRequest \req -> runBasicAuth req realm basicAuthContext


allowedMethodHead :: Method -> Request -> Boolean
allowedMethodHead method (Request req) = method == methodGet && (show req.method) == methodHead

allowedMethod :: Method -> Request -> Boolean
allowedMethod method request@(Request req) = allowedMethodHead method request || (show req.method) == method

methodCheck :: Method -> Request -> DelayedIO Unit
methodCheck method request
  | allowedMethod method request = pure unit 
  | otherwise                    = delayedFail err405

acceptCheck :: forall list. AllMime list => Proxy list -> AcceptHeader -> DelayedIO Unit
acceptCheck proxy accH
  | canHandleAcceptH proxy accH = pure unit
  | otherwise                   = delayedFail err406

ctWildcard :: String
ctWildcard = "*" <> "/" <> "*" 

getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader (Request req) = AcceptHeader <<< fromMaybe ctWildcard <<< Map.lookup hAccept $ requestHeaders
  where
    requestHeaders = Map.fromFoldable req.headers
