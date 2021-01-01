module Swerve.Server.Internal.Eval where 

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Network.Wai (Application)
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
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

-- Server type to be used as an associated type.
type Server spec = ServerT spec Aff 
data ServerT (api :: Type) (m :: Type -> Type) 

-- Server Eval 
class EvalServer (a :: Type) (b :: Type) | a -> b

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
                 (Delayed env (m waiApp))
             
instance evalDelayedBasicAuth
  :: EvalDelayed (Delayed env (ServerT (BasicAuth realm usr :> api) m)) 
                 (Delayed env (usr -> ServerT api m))

instance evalDelayedAuth
  :: EvalDelayed (Delayed env (ServerT (AuthProtect tag :> api) m)) 
                 (Delayed env (a -> ServerT api m))

-- Handler Eval
class EvalHandler (a :: Type) (b :: Type) | a -> b

instance evalAltHandlers' :: 
  ( EvalHandler a a'
  , EvalHandler  b b'
  ) => EvalHandler (a :<|> b) (a' :<|> b')

instance evalHandlerPath :: 
  ( IsSymbol path 
  , EvalHandler api server
  ) => EvalHandler (path :> api) server

instance evalHandlerReqBody 
  :: EvalHandler api server 
  => EvalHandler (ReqBody ctypes a :> api) (a -> server)

instance evalHandlerCapture
  :: EvalHandler api server 
  => EvalHandler (Capture sym a :> api) (a -> server)

instance evalHandlerHeader
  :: EvalHandler api server 
  => EvalHandler (Header sym a :> api) (a -> server)

instance evalHandlerQueryParam
  :: EvalHandler api server 
  => EvalHandler (QueryParam sym a :> api) (Maybe a -> server)

instance evalHandlerSVerb :: EvalHandler (Verb method ctypes as) (m (Variant as))

instance evalHandlerRaw 
  :: TypeEquals Application waiApp 
  => EvalHandler Raw (m waiApp)

instance evalHandlerBasicAuth
  :: EvalHandler (ServerT api m) server 
  => EvalHandler (ServerT (BasicAuth realm usr :> api) m) (usr -> server)

instance evalHandlerAuth 
  :: EvalHandler (ServerT api m) server 
  => EvalHandler (ServerT (AuthProtect tag :> api) m) (a -> server)

evalD :: forall a b. EvalDelayed a b => a -> b
evalD = unsafeCoerce

eval :: forall a b. EvalServer a b => a -> b
eval = unsafeCoerce

lift :: forall a b. EvalServer b a => a -> b
lift = unsafeCoerce