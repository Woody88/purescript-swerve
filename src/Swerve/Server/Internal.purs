module Swerve.Server.Internal where

import Prelude

import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple.Nested ((/\))
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
import Swerve.API.Auth (AuthProtect)
import Swerve.API.BasicAuth (BasicAuth)
import Swerve.API.Capture (class ReadCapture, readCapture)
import Swerve.API.ContentType (class Accepts, class AllCTRender, class AllCTUnrender, class AllMime, AcceptHeader(..), handleAcceptH, canHandleAcceptH, canHandleCTypeH)
import Swerve.API.Header (class ReadHeader, readHeader)
import Swerve.API.Method (class ToMethod, toMethod)
import Swerve.API.QueryParam (class ReadQuery, readQuery)
import Swerve.API.Status (class HasStatus)
import Swerve.API.Types (type (:<|>), type (:>), Capture, Header, QueryParam, Raise, Raw, ReqBody, Respond', Verb, (:<|>))
import Swerve.Server.Internal.Auth (AuthHandler, unAuthHandler)
import Swerve.Server.Internal.BasicAuth (BasicAuthCheck, runBasicAuth)
import Swerve.Server.Internal.Delayed (Delayed, addAuthCheck, addAcceptCheck, addBodyCheck, addCapture, addHeaderCheck, addMethodCheck, addParameterCheck, runAction, runDelayed)
import Swerve.Server.Internal.DelayedIO (DelayedIO, delayedFail, delayedFailFatal, withRequest)
import Swerve.Server.Internal.Eval (class EvalServer, Server, ServerT, lift, eval, evalD)
import Swerve.Server.Internal.Response (RespondData, Response(..))
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.Router (Router, Router'(..), choice, leafRouter, pathRouter)
import Swerve.Server.Internal.ServerError (ServerError, err400, err405, err406, err415, err500)
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEquals 
import Type.Proxy (Proxy(..))
import Type.Row as Row
import Unsafe.Coerce

-- data Server' (api :: Type)  (m :: Type -> Type) 

-- type Server spec = Server' spec Aff

-- class HasServer :: Type -> Row Type -> (Type -> Type) -> Type -> Constraint
-- class HasServer api context m handler | api -> handler context where 
--   route :: forall env. Proxy api -> Proxy m -> Record context -> Delayed env (Server api) -> Router env
--   hoistServerWithContext :: forall n. Proxy api -> Proxy context -> (forall x. m x -> n x) -> Server' api m -> Server' api n

class HasServer api context m | api -> context m where 
  route :: forall env. Proxy api -> Proxy m -> Record context -> Delayed env (Server api) -> Router env
  hoistServerWithContext :: forall n. Proxy api -> Proxy context -> (m ~> n) -> ServerT api m -> ServerT api n

instance hasServerAlt :: 
  ( HasServer a context m
  , HasServer b context m
  ) => HasServer (a :<|> b) context m where 
  hoistServerWithContext _ pc nt server = let 
    (servera :<|> serverb) = (eval server)
    in 
      lift (hoistServerWithContext (Proxy :: _ a) pc nt servera :<|> hoistServerWithContext (Proxy :: _ b) pc nt serverb)

  route _ m context server = choice (route pa m context ((\ (a :<|> _) -> a) <$> evalD server))
                                    (route pb m context ((\ (_ :<|> b) -> b) <$> evalD server))
    where
      pa = Proxy :: _ a 
      pb = Proxy :: _ b 

instance hasServerVerb :: 
  ( HasStatus status label
  , ToMethod method
  , Accepts ctypes
  , AllCTRender ctypes a
  ) => HasServer (Verb method a status hdrs ctypes) context m where 
  hoistServerWithContext _ _ nt server = lift $ nt $ eval server
  route _ _ _ subserver = leafRouter route'
    where 
      status = Proxy :: _ status 
      method = toMethod (Proxy :: _ method)
      ctypesP = Proxy :: _ ctypes 
      route' env request respond = do 
        let 
          accH   = getAcceptHeader request
          action = (evalD subserver) `addMethodCheck` methodCheck method request
                                       `addAcceptCheck` acceptCheck ctypesP accH
        runAction action env request respond 
          \(Response v) -> case v of 
              Left f  -> Route $ responseStr f.status [] f.content
              Right (s :: RespondData a) -> case handleAcceptH ctypesP accH s.content of 
                Nothing -> FailFatal err406 
                Just (ct /\ body) -> 
                  let bdy     = if allowedMethodHead method request then "" else body
                      headers = (hContentType /\ ct) :  s.headers 
                  in Route $ responseStr s.status headers bdy

instance hasServerRaise :: 
  ( HasStatus status label
  , HasServer api context m 
  ) => HasServer (Raise status hdrs ctypes :> api) context m where 
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: Proxy api) pc nt (eval s)
  route _ m ctx subserver = route (Proxy :: _ api) m ctx (evalD subserver) 

instance hasServerSegment :: 
  ( IsSymbol path
  , HasServer api context m
  ) => HasServer (path :> api) context m where 
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: _ api) pc nt (eval s)
  route _ m ctx subserver = pathRouter path (route (Proxy :: _ api) m ctx (evalD subserver))
    where 
      path = reflectSymbol (SProxy :: _ path) 

instance hasServerRaw :: HasServer Raw context m where 
  hoistServerWithContext _ _ f m  = lift (f $ eval m)
  route _ _ _ rawApplication = RawRouter $ \env request respond -> do   
    r <- runDelayed (evalD rawApplication) env request
    case r of 
      Route appM -> do 
        app <- appM
        app request (respond <<< Route)
      Fail a      -> respond $ Fail a 
      FailFatal e -> respond $ FailFatal e

instance hasServerReqBody :: 
  ( AllCTUnrender ctypes a
  , HasServer api context m 
  ) => HasServer (ReqBody a ctypes :> api) context m where 
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (eval s)
  route _ m context subserver = route (Proxy :: Proxy api) m context $ addBodyCheck (evalD subserver) ctCheck bodyCheck
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
  , HasServer api context m 
  ) => HasServer (Capture a :> api) context m where 
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (eval s)

  route _ m ctx subserver =
    CaptureRouter $ 
      route (Proxy :: _ api) m ctx
        (addCapture (evalD subserver) \c -> withRequest \request -> 
          case readCapture c of 
            Nothing           -> delayedFail err400 
            Just (piece :: a) -> pure piece)

instance hasServerHeader :: 
  ( IsSymbol sym
  , ReadHeader a
  , HasServer api context m
  ) => HasServer (Header sym a :> api) context m where 
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (eval s)
  route _ m ctx subserver = route (Proxy :: _ api) m ctx delayed
    where 
      delayed = addHeaderCheck (evalD subserver) <<< withRequest $ 
        \(Request req) -> let 
          key = reflectSymbol (SProxy :: _ sym)
          hMap = Map.fromFoldable $ req.headers 
          in case Map.lookup (wrap key) hMap >>= readHeader of 
            Nothing -> delayedFail err400 { content = req.url }
            Just (piece :: a) -> pure piece

instance hasServerQuery :: 
  ( IsSymbol sym
  , ReadQuery a
  , HasServer api context m
  ) => HasServer (QueryParam sym a :> api) context m where 
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (eval s)
  route _ m ctx subserver = route (Proxy :: _ api) m ctx delayed
    where 
      delayed = addParameterCheck (evalD subserver) <<< withRequest $ 
        \(Request req) -> let 
          key = reflectSymbol (SProxy :: _ sym)
          qMap = Map.fromFoldable $ req.queryString
          in case Map.lookup key qMap of 
            Nothing      -> delayedFail err400 { content = req.url }
            Just mParam  -> case mParam of 
              Nothing    -> delayedFail err400 { content = req.url } 
              Just param -> pure $ readQuery param

instance hasServerBasicAuth ::
  ( IsSymbol realm 
  , HasServer api (basicAuth :: BasicAuthCheck usr | context) m 
  ) => HasServer (BasicAuth realm usr :> api) (basicAuth :: BasicAuthCheck usr | context) m where
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: Proxy api) pc nt <<< (eval s)
  route _ m ctx subserver = route (Proxy :: Proxy api) m ctx ((evalD subserver) `addAuthCheck` authCheck)
    where
       realm = reflectSymbol (SProxy :: _ realm)
       basicAuthContext = Record.get (SProxy :: _ "basicAuth") ctx
       authCheck = withRequest \req -> runBasicAuth req realm basicAuthContext

instance hasServerAuth ::
  ( IsSymbol tag 
  , HasServer api context m
  , Row.Cons tag (AuthHandler Request a) r context 
  ) => HasServer (AuthProtect tag :> api) context m where
  hoistServerWithContext _ pc nt s = lift $ hoistServerWithContext (Proxy :: _ api) pc nt <<< (eval s)
  route _ m ctx subserver = route (Proxy :: Proxy api) m ctx ((evalD subserver) `addAuthCheck` (withRequest authCheck))
    where 
      authCheck :: Request -> DelayedIO (Aff a) 
      authCheck req = (liftAff $ authHandler req) >>= (either delayedFailFatal (pure <<< pure))

      authHandler :: Request -> Aff (Either ServerError a)
      authHandler = unAuthHandler $ Record.get (SProxy :: _ tag) ctx

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