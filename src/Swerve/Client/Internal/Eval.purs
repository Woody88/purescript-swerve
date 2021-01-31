module Swerve.Client.Internal.Eval where 

import Data.Symbol
import Data.Variant (Variant)
import Network.HTTP.Types.Method as H
import Network.Wai (Application)
import Prim.RowList as RL
import Swerve.API.Alternative (type (:<|>))
import Swerve.API.Auth
import Swerve.API.BasicAuth
import Swerve.API.Capture
import Swerve.API.Header
import Swerve.API.QueryParam
import Swerve.API.Raw
import Swerve.API.ReqBody
import Swerve.API.Sub 
import Swerve.API.Verb (Verb)
import Swerve.Client.Internal.Auth (AuthenticatedRequest)
import Swerve.Client.Internal.Response 
import Type.Equality (class TypeEquals)
import Type.RowList (class RowToList, class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

data Client (m :: Type -> Type) (api :: Type)

class EvalClient (a :: Type) (b :: Type) | a -> b

instance _evalClientRec :: EvalClient (Client m (Record routes)) (Record routes')

instance _evalClientAlt :: EvalClient (Client m (a :<|> b) ) ((Client m a) :<|> (Client m b))

instance _evalClientVerb :: EvalClient (Client m (Verb method cts as)) (m (Variant as))

instance _evalClientCapture :: EvalClient (Client m (Capture name a :> api)) (a -> Client m api)

instance _evalClientHeader :: EvalClient (Client m (Header name a :> api)) (a -> Client m api)

instance _evalClientQueryParam :: EvalClient (Client m (QueryParam name a :> api)) (a -> Client m api)

instance _evalClientReqBody :: EvalClient (Client m (ReqBody ctypes a :> api)) (a -> Client m api)

instance _evalClientBasicAuth :: EvalClient (Client m (BasicAuth realm usr :> api)) (BasicAuthData -> Client m api)

instance _evalClientAuth :: EvalClient (Client m (AuthProtect tag :> api)) (AuthenticatedRequest a -> Client m api)

instance _evalClientRaw :: EvalClient (Client m Raw) (H.StdMethod -> m Response)

instance _evalClientPath 
  :: IsSymbol path 
  => EvalClient (Client m (path :> api)) (Client m api)


class EvalRoutes a b | a -> b

instance evalRoutesNil' :: EvalRoutes RL.Nil RL.Nil 

instance evalRoutesConst :: 
  ( EvalHandler a a'
  , EvalRoutes rest rest'
  ) => EvalRoutes (RL.Cons name a rest) (RL.Cons name a' rest') 


class EvalHandler (a :: Type) (b :: Type) | a -> b

instance evalRoutesHandlers :: 
  ( RowToList routes rl 
  , EvalRoutes rl rl'  
  , ListToRow rl' routes'
  ) => EvalHandler (Record routes) (Record routes')

instance evalHandlerCapture
  :: EvalHandler api server 
  => EvalHandler (Capture sym a :> api) (a -> server)

instance evalHandlerHeader
  :: EvalHandler api server 
  => EvalHandler (Header sym a :> api) (a -> server)

instance evalHandlerQueryParam
  :: EvalHandler api server 
  => EvalHandler (QueryParam sym a :> api) (a -> server)

instance evalHandlerReqBody
  :: EvalHandler api server 
  => EvalHandler (ReqBody ctypes a :> api) (a -> server)

instance evalHandlerBasicAuth
  :: EvalHandler api server 
  => EvalHandler (BasicAuth realm usr :> api) (BasicAuthData -> server)

instance evalHandlerAuth
  :: EvalHandler api server 
  => EvalHandler (AuthProtect tag :> api) (AuthenticatedRequest a -> server)

instance evalHandlerRaw 
  :: TypeEquals Application waiApp 
  => EvalHandler Raw (H.StdMethod -> m waiApp)

instance evalHandlerPath :: 
  ( IsSymbol path 
  , EvalHandler api server
  ) => EvalHandler (path :> api) server

instance evalHandlerSVerb :: EvalHandler (Verb method ctypes as) (m (Variant as))

eval :: forall a b. EvalClient a b => a -> b
eval = unsafeCoerce

lift :: forall a b. EvalClient b a => a -> b
lift = unsafeCoerce