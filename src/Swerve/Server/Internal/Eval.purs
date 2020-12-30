module Swerve.Server.Internal.Eval where 

import Data.Either
import Data.Maybe 
import Data.Symbol
import Effect.Aff (Aff)
import Network.Wai (Application, Request)
import Swerve.API.Auth
import Swerve.API.BasicAuth
import Swerve.API.Types
import Swerve.Server.Internal.Auth
import Swerve.Server.Internal.Delayed (Delayed)
import Swerve.Server.Internal.Response
import Swerve.Server.Internal.Handler 
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

data ServerT (api :: Type)  (m :: Type -> Type) 

type Server spec = ServerT spec Handler 

class EvalServer a b | a -> b 


instance evalAltHandlers :: 
  ( EvalServer (ServerT a m) a'
  , EvalServer (ServerT b m) b'
  ) => EvalServer (ServerT (a :<|> b) m) (a' :<|> b')

else instance evalPath :: 
  ( IsSymbol path 
  -- , EvalServer (ServerT api m) server
  ) => EvalServer (ServerT (path :> api) m) (ServerT api m)


else instance evalCapture
  :: EvalServer (ServerT api m) server 
  => EvalServer (ServerT (Capture a :> api) m) (a -> server)

else instance evalVerb :: EvalServer (ServerT (Verb method a status hdrs ct) m) (m a)

else instance evalI :: EvalServer a b

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

-- instance evalDelayedReqBody 
--   :: EvalServer' (Delayed env (ServerT ((ReqBody a ctypes) :> api) m)) 
--                  (Delayed env (a -> ServerT api m))

-- instance evalDelayedRaise :: EvalServer' (Delayed env (ServerT ((Raise status hdrs ctypes) :> api) m))
--                                          (Delayed env (ServerT api m))

instance evalDelayedCapture 
  :: EvalDelayed (Delayed env (ServerT (Capture a :> api) m)) 
                 (Delayed env (a -> ServerT api m))


instance evalDelayedVerb
  :: EvalDelayed (Delayed env (ServerT (Verb method a status hdrs ctypes) m)) 
                 (Delayed env (m (Response rs a)))

-- instance evalDelayedHeader
--   :: EvalServer' (Delayed env (ServerT (Header sym a :> api) m)) 
--                  (Delayed env (a -> ServerT api m))

-- instance evalDelayedQueryParam
--   :: EvalServer' (Delayed env (ServerT (QueryParam sym a :> api) m)) 
--                  (Delayed env (Maybe a -> ServerT api m))
             
-- instance evalDelayedBasicAuth
--   :: EvalServer' (Delayed env (ServerT (BasicAuth realm usr :> api) m)) 
--                  (Delayed env (usr -> ServerT api m))

-- instance evalDelayedAuth
--   :: EvalServer' (Delayed env (ServerT (AuthProtect tag :> api) m)) 
--                  (Delayed env (m a -> ServerT api m))

-- Server Compose Eval 
instance evalAltServers :: EvalHandler (ServerT a m :<|> ServerT b m) (ServerT (a :<|> b) m)


-- Server Eval

instance evalAltHandlers' :: 
  ( EvalHandler (ServerT a m) a'
  , EvalHandler (ServerT b m) b'
  ) => EvalHandler (ServerT (a :<|> b) m) (a' :<|> b')

class EvalHandler a b | a -> b

instance evalPath' :: 
  ( IsSymbol path 
  , EvalHandler api server
  ) => EvalHandler (path :> api) server

instance evalCapture'
  :: EvalHandler api server 
  => EvalHandler (Capture a :> api) (a -> server)

instance evalVerb' :: EvalHandler (Verb method a status hdrs ct) (m a)

-- instance evalRaw 
--   :: TypeEquals Application application 
--   => EvalServer' (ServerT Raw m) (m application)

-- instance evalRaise 
--   :: EvalServer (ServerT api m) server 
--   => EvalServer' (ServerT ((Raise status hdrs ctypes) :> api) m) server

-- instance evalReqBody 
--   :: EvalServer (ServerT api m) server 
--   => EvalServer' (ServerT (ReqBody a ctypes :> api) m) (a -> server)



-- instance evalHeader
--   :: EvalServer (ServerT api m) server 
--   => EvalServer' (ServerT (Header sym a :> api) m) (a -> server)

-- instance evalQueryParam
--   :: EvalServer (ServerT api m) server 
--   => EvalServer' (ServerT (QueryParam sym a :> api) m) (Maybe a -> server)

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

-- eval :: forall a b. EvalServer' a b => a -> b
-- eval = unsafeCoerce

lift :: forall a b. EvalServer b a => a -> b
lift = unsafeCoerce

compose :: forall a b. EvalServer b a => b -> a
compose = unsafeCoerce