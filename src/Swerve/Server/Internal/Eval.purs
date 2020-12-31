module Swerve.Server.Internal.Eval where 

import Data.Either
import Data.Maybe 
import Data.Symbol
import Effect.Aff (Aff)
import Network.Wai (Application, Request)
import Swerve.API.Auth
import Swerve.API.BasicAuth
import Swerve.API.Types
import Swerve.API.Status (class HasStatus)
import Swerve.Server.Internal.Auth
import Swerve.Server.Internal.Delayed (Delayed)
import Swerve.Server.Internal.Response
import Swerve.Server.Internal.Handler 
import Type.Equality (class TypeEquals)
import Type.Row as Row
import Unsafe.Coerce (unsafeCoerce)

data ServerT (api :: Type)  (m :: Type -> Type) 


type Server spec = ServerT spec Aff 

class EvalServer a b | a -> b

instance evalServerAlt :: EvalServer (ServerT (a :<|> b) m) ((ServerT a m) :<|> (ServerT b m))

instance evalServerAlt' :: EvalServer ((ServerT a m) :<|> (ServerT b m)) (ServerT (a :<|> b) m)

instance evalServerPath 
  :: IsSymbol path 
  => EvalServer (ServerT (path :> api) m) (ServerT api m) 

instance evalServerReqBody 
  :: EvalServer (ServerT (ReqBody a ctypes :> api) m) (a -> (ServerT api m))

instance evalServerCapture 
  :: EvalServer (ServerT (Capture a :> api) m) (a -> (ServerT api m))

instance evalServerHeader 
  :: IsSymbol sym
  => EvalServer (ServerT (Header sym a :> api) m) (a -> (ServerT api m))

instance evalServerQueryParam 
  :: IsSymbol sym
  => EvalServer (ServerT (QueryParam sym a :> api) m) (Maybe a -> (ServerT api m))

instance evalServerRaise 
  :: EvalServer (ServerT (Raise status hdrs ctypes :> api) m)
                (ServerT api m)

instance evalServerVerb 
  :: EvalServer (ServerT (Verb method a status hdrs ctypes) m) (m a)

instance evalServerRaw 
  :: TypeEquals Application waiApp 
  => EvalServer (ServerT Raw m) (m waiApp)

instance evalServerBasicAuth
  :: EvalServer (ServerT (BasicAuth realm usr :> api) m) 
                (usr -> ServerT api m)

instance evalServerAuth
  :: EvalServer (ServerT (AuthProtect tag :> api) m)
                (a -> ServerT api m)


-- instance evalServerSub :: EvalServer a b

-- toHoistServer :: forall server hoistServer. EvalServer server hoistServer => server -> hoistServer
-- toHoistServer = unsafeCoerce

-- toHandler :: forall api api' env. EvalServer api api' => Delayed env api -> Delayed env api'
-- toHandler = unsafeCoerce

-- class EvalServer' a b | a -> b

class EvalDelayed a b | a -> b


-- Delayed Eval 
instance evalAltDelayed 
  :: EvalDelayed (Delayed env (ServerT (a :<|> b) m))  
                 (Delayed env (ServerT a m :<|> ServerT b m)) 
                  

instance evalDelayedPath 
  :: IsSymbol path 
  => EvalDelayed (Delayed env (ServerT (path :> api) m)) (Delayed env (ServerT api m))

instance evalDelayedReqBody 
  :: EvalDelayed (Delayed env (ServerT (ReqBody a ctypes :> api) m)) 
                 (Delayed env (a -> ServerT api m))

instance evalDelayedRaise 
  :: EvalDelayed (Delayed env (ServerT (Raise status hdrs ctypes :> api) m))
                 (Delayed env (ServerT api m))

instance evalDelayedCapture 
  :: EvalDelayed (Delayed env (ServerT (Capture a :> api) m)) 
                 (Delayed env (a -> ServerT api m))

instance evalDelayedHeader 
  :: IsSymbol sym
  => EvalDelayed (Delayed env (ServerT (Header sym a :> api) m)) 
                 (Delayed env (a -> ServerT api m))

instance evalDelayedQueryParam
  :: IsSymbol sym
  => EvalDelayed (Delayed env (ServerT (QueryParam sym a :> api) m)) 
                 (Delayed env (Maybe a -> ServerT api m))

instance evalDelayedVerb
  :: EvalDelayed (Delayed env (ServerT (Verb method a status hdrs ctypes) m)) 
                 (Delayed env (m (Response rs a)))

instance evalDelayedRaw
  :: TypeEquals Application waiApp 
  => EvalDelayed (Delayed env (ServerT Raw m)) 
                 (Delayed env (m waiApp))
             
instance evalDelayedBasicAuth
  :: EvalDelayed (Delayed env (ServerT (BasicAuth realm usr :> api) m)) 
                 (Delayed env (usr -> ServerT api m))

instance evalDelayedAuth
  :: EvalDelayed (Delayed env (ServerT (AuthProtect tag :> api) m)) 
                 (Delayed env (a -> ServerT api m))

-- Server Compose Eval 
-- instance evalAltServers :: EvalHandler (ServerT a m :<|> ServerT b m) (ServerT (a :<|> b) m)


-- Server Eval

instance evalAltHandlers' :: 
  ( EvalHandler a a'
  , EvalHandler  b b'
  ) => EvalHandler (a :<|> b) (a' :<|> b')

class EvalHandler a b | a -> b

instance evalHandlerPath :: 
  ( IsSymbol path 
  , EvalHandler api server
  ) => EvalHandler (path :> api) server

instance evalHandlerReqBody 
  :: EvalHandler api server 
  => EvalHandler (ReqBody a ctypes :> api) (a -> server)

instance evalHandlerCapture
  :: EvalHandler api server 
  => EvalHandler (Capture a :> api) (a -> server)

instance evalHandlerHeader
  :: EvalHandler api server 
  => EvalHandler (Header sym a :> api) (a -> server)

instance evalHandlerQueryParam
  :: EvalHandler api server 
  => EvalHandler (QueryParam sym a :> api) (Maybe a -> server)

instance evalHandlerRaise :: 
  ( HasStatus status label
  , EvalHandler api server 
  , Row.Cons label (Respond status () ct) r rs
  )=> EvalHandler (Raise status hdrs ctypes :> api) server

instance evalHandlerVerb :: EvalHandler (Verb method a status hdrs ct) (m (Response rs a))

instance evalHandlerRaw 
  :: TypeEquals Application waiApp 
  => EvalHandler Raw (m waiApp)

-- instance evalBasicAuth
--   :: EvalServer (ServerT api m) server 
--   => EvalServer' (ServerT (BasicAuth realm usr :> api) m) (usr -> server)

-- instance evalAuth :: 
--   ( EvalServer (ServerT api m) server 
--   ) => EvalServer' (ServerT (AuthProtect tag :> api) m) (a -> server)



evalD :: forall a b. EvalDelayed a b => a -> b
evalD = unsafeCoerce

eval :: forall a b. EvalServer a b => a -> b
eval = unsafeCoerce

lift :: forall a b. EvalServer b a => a -> b
lift = unsafeCoerce