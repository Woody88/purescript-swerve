module Swerve.Server.Internal.Eval where 

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Network.Wai (Application)
import Prim.Row as Row 
import Prim.RowList as RL
import Swerve.API.Alternative (type (:<|>))
import Swerve.API.Auth (AuthProtect)
import Swerve.API.BasicAuth (BasicAuth)
import Swerve.API.Capture (Capture)
import Swerve.API.Header (Header)
import Swerve.API.QueryParam (QueryParam)
import Swerve.API.Sub (type (:>))
import Swerve.API.Raw (Raw)
import Swerve.API.ReqBody  (ReqBody)
import Swerve.API.Verb (Verb)
import Swerve.Server.Internal.Delayed (Delayed)
import Swerve.Server.Internal.Handler (HandlerM)
import Type.Equality (class TypeEquals)
import Type.RowList (class RowToList, class ListToRow)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- Server type to be used as an associated type.
type Server spec = ServerT spec HandlerM 
data ServerT (api :: Type) (m :: Type -> Type) 

-- Server Eval 
class EvalServer (a :: Type) (b :: Type) | a -> b

instance evalServerRec :: EvalServer (ServerT (Record routes) m) (ServerT (Record routes) m)

instance evalServerAlt :: EvalServer (ServerT (a :<|> b) m) ((ServerT a m) :<|> (ServerT b m))

instance evalServerAlt' :: EvalServer ((ServerT a m) :<|> (ServerT b m)) (ServerT (a :<|> b) m)

instance evalServerPath 
  :: IsSymbol path 
  => EvalServer (ServerT (path :> api) m) (ServerT api m) 

instance evalServerReqBody 
  :: EvalServer (ServerT (ReqBody ctypes a :> api) m) (a -> (ServerT api m))

instance evalServerCapture 
  :: EvalServer (ServerT (Capture sym a :> api) m) (a -> (ServerT api m))

instance evalServerHeader 
  :: IsSymbol sym
  => EvalServer (ServerT (Header sym a :> api) m) (a -> (ServerT api m))

instance evalServerQueryParam 
  :: IsSymbol sym
  => EvalServer (ServerT (QueryParam sym a :> api) m) (Maybe a -> (ServerT api m))

instance evalServerSVerb 
  :: EvalServer (ServerT (Verb method ctypes as) m) (m (Variant as))

instance evalServerRaw 
  :: TypeEquals Application waiApp 
  => EvalServer (ServerT Raw m) (m waiApp)

instance evalServerBasicAuth
  :: EvalServer (ServerT (BasicAuth realm usr :> api) m) 
                (usr -> ServerT api m)

instance evalServerAuth
  :: EvalServer (ServerT (AuthProtect tag :> api) m)
                (a -> ServerT api m)

-- Delayed Eval 
class EvalDelayed (a :: Type) (b :: Type) | a -> b


instance evalConsDelayed 
  ::  EvalDelayed (Delayed env (ServerT (Proxy rl) m))
                 (Delayed env (Record row))

instance evalRoutesDelayed 
  :: EvalDelayed (Delayed env (ServerT (Record routes) m))
                 (Delayed env (Record routes))

instance evalAltDelayed 
  :: EvalDelayed (Delayed env (ServerT (a :<|> b) m))  
                 (Delayed env (ServerT a m :<|> ServerT b m)) 
                  
instance evalDelayedPath 
  :: IsSymbol path 
  => EvalDelayed (Delayed env (ServerT (path :> api) m)) (Delayed env (ServerT api m))

instance evalDelayedReqBody 
  :: EvalDelayed (Delayed env (ServerT (ReqBody ctypes a :> api) m)) 
                 (Delayed env (a -> ServerT api m))

instance evalDelayedCapture 
  :: EvalDelayed (Delayed env (ServerT (Capture sym a :> api) m)) 
                 (Delayed env (a -> ServerT api m))

instance evalDelayedHeader 
  :: IsSymbol sym
  => EvalDelayed (Delayed env (ServerT (Header sym a :> api) m)) 
                 (Delayed env (a -> ServerT api m))

instance evalDelayedQueryParam
  :: IsSymbol sym
  => EvalDelayed (Delayed env (ServerT (QueryParam sym a :> api) m)) 
                 (Delayed env (Maybe a -> ServerT api m))

instance evalDelayedSVerb
  :: EvalDelayed (Delayed env (ServerT (Verb method ctype as) m)) 
                 (Delayed env (m (Variant as)))

instance evalDelayedRaw
  :: TypeEquals Application waiApp 
  => EvalDelayed (Delayed env (ServerT Raw m)) 
                 (Delayed env (n waiApp))
             
instance evalDelayedBasicAuth
  :: EvalDelayed (Delayed env (ServerT (BasicAuth realm usr :> api) m)) 
                 (Delayed env (usr -> ServerT api m))

instance evalDelayedAuth
  :: EvalDelayed (Delayed env (ServerT (AuthProtect tag :> api) m)) 
                 (Delayed env (a -> ServerT api m))

data Regular 
data Compose 

class EvalRoutes m a b | a -> b

instance evalRoutesNil' :: EvalRoutes Regular RL.Nil RL.Nil 

instance evalRoutesNil :: EvalRoutes Compose RL.Nil RL.Nil 

instance evalRoutesConst' :: 
  ( EvalRoutes Compose rest rest'
  , EvalCompose a a'
  ) => EvalRoutes Compose (RL.Cons name a rest) (RL.Cons name (ServerT a' m) rest') 

instance evalRoutesConst :: 
  ( EvalHandler Regular a a'
  , EvalRoutes Regular rest rest'
  ) => EvalRoutes Regular (RL.Cons name a rest) (RL.Cons name a' rest') 


class EvalCompose a b | a -> b

instance evalComposeRec :: EvalCompose (Record a) (Record a)

instance evalComposePath :: 
  ( IsSymbol path 
  , EvalCompose api server
  ) => EvalCompose (path :> api) server

instance evalComposeRaw 
  :: EvalCompose Raw raw

instance evalComposeBasicAuth
  :: EvalCompose api server 
  => EvalCompose (BasicAuth realm usr :> api) (usr -> server)

instance evalComposeSVerb :: EvalCompose (Verb method ctypes as) (m (Variant as))

instance evalComposeAuth 
  :: EvalCompose api server 
  => EvalCompose (AuthProtect tag :> api) (a -> server)

-- Handler Eval
class EvalHandler m (a :: Type) (b :: Type) | a -> b

instance evalRoutesHandlers :: 
  ( RowToList routes rl 
  , EvalRoutes Regular rl rl'  
  , ListToRow rl' routes'
  ) => EvalHandler Regular (Record routes) (Record routes')

instance evalRoutesHandlers' :: 
  ( RowToList routes rl 
  , EvalRoutes Compose rl rl'  
  , ListToRow rl' routes'
  ) => EvalHandler Compose (Record routes) (Record routes')

instance evalAltHandlers :: 
  ( EvalHandler Regular a a' 
  ,  EvalHandler Regular b b' 
  ) => EvalHandler Regular (a :<|> b) (a' :<|>  b')

instance evalAltHandlers' :: EvalHandler Compose (a :<|> b) (ServerT a m :<|>  ServerT b m)

instance evalHandlerPath :: 
  ( IsSymbol path 
  , EvalHandler m api server
  ) => EvalHandler m (path :> api) server

instance evalHandlerReqBody 
  :: EvalHandler m api server 
  => EvalHandler m (ReqBody ctypes a :> api) (a -> server)

instance evalHandlerCapture
  :: EvalHandler m api server 
  => EvalHandler m (Capture sym a :> api) (a -> server)

instance evalHandlerHeader
  :: EvalHandler m api server 
  => EvalHandler m (Header sym a :> api) (a -> server)

instance evalHandlerQueryParam
  :: EvalHandler m api server 
  => EvalHandler m (QueryParam sym a :> api) (Maybe a -> server)

instance evalHandlerSVerb :: EvalHandler n (Verb method ctypes as) (m (Variant as))

instance evalHandlerRaw 
  :: TypeEquals Application waiApp 
  => EvalHandler n Raw (m waiApp)

instance evalHandlerBasicAuth
  :: EvalHandler m api server 
  => EvalHandler m (BasicAuth realm usr :> api) (usr -> server)

instance evalHandlerAuth 
  :: EvalHandler m api server 
  => EvalHandler m (AuthProtect tag :> api) (a -> server)

evalD :: forall a b. EvalDelayed a b => a -> b
evalD = unsafeCoerce

eval :: forall a b. EvalServer a b => a -> b
eval = unsafeCoerce

lift :: forall a b. EvalServer b a => a -> b
lift = unsafeCoerce