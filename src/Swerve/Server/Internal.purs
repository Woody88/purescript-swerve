module Swerve.Server.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff (Aff)
import Network.Wai (Application, responseStr)
import Prim.RowList (class RowToList)
import Simple.JSON (class WriteForeign, writeJSON)
import Swerve.API.Status (class HasStatus, getStatus)
import Swerve.API.Types (type (:>), Spec, Verb)
import Swerve.Server.Internal.Delayed (Delayed, emptyDelayed, runAction)
import Swerve.Server.Internal.Response (class VariantResponse, Response(..), variantResponse)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.Router (Router, leafRouter, pathRouter, runRouter)
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Swerve.Server.Internal.ServerError (err404)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Server' ( spec :: Spec) (m :: Type -> Type)

type Server spec = Server' spec Aff

class HasServer :: forall k. Spec -> Row Type -> k -> Constraint
class HasServer api context handler | api -> context handler where 
  route :: forall env. Proxy api -> Record context -> Delayed env (Server api) -> Router env

instance hasServerVerb :: 
  ( RowToList row rl
  , HasStatus status label 
  , WriteForeign a 
  , VariantResponse row rl 
  ) => HasServer (Verb status a hdrs ctypes row) context (Aff (Response row a)) where 
  route _ _ subserver = leafRouter route'
    where 
      status = Proxy :: _ status 
      route' env request respond = runAction (toHandler subserver) env request respond 
        \(Response v) -> case v of 
            Right a -> Route $ responseStr (getStatus status) [] (writeJSON a)
            Left vr -> Route $ variantResponse (Proxy :: _ rl) vr

instance hasServerSegment :: 
  ( IsSymbol path
  , HasServer api context handler'
  ) => HasServer (path :> api) context handler where 
  route _ ctx subserver = pathRouter path (route (Proxy :: _ api) ctx (toHandler subserver))
    where 
      path = reflectSymbol (SProxy :: _ path)

from :: forall handlers api context. HasServer api context handlers => handlers -> Server api
from = unsafeCoerce

toHandler :: forall api handler env context. HasServer api context handler => Delayed env (Server api) -> Delayed env handler
toHandler = unsafeCoerce

serve :: forall api handler. 
  HasServer api () handler 
  => Proxy api -> Server api -> Application
serve p serv = toApplication (runRouter (const err404) (route p {} (emptyDelayed (Route serv))))

serve' :: forall api handler context. 
  HasServer api context handler 
  => Proxy api -> Record context -> Server api -> Application
serve' p ctx serv = toApplication (runRouter (const err404) (route p ctx (emptyDelayed (Route serv))))