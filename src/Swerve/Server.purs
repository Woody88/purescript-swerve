module Swerve.Server 
  ( module Response
  , module ServerType
  , serve
  , serve' 
  , hoistServer
  , from 
  ) where

import Prelude 

import Effect.Aff (Aff)
import Network.Wai (Application) 
import Swerve.Server.Internal (Server, Server') as ServerType
import Swerve.Server.Internal (class HasServer, Server, Server', hoistServerWithContext, route)
import Swerve.Server.Internal.Delayed (emptyDelayed)
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Swerve.Server.Internal.Router (runRouter)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.ServerError (err404)
import Swerve.Server.Internal.Response (class HasResp, Response(..), raise, respond) as Response
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

serve :: forall api handler. 
  HasServer api () Aff handler
  => Proxy api -> Server api -> Application
serve p s = toApplication (runRouter (const err404) (route p (Proxy :: _ Aff) {} (emptyDelayed (Route s))))

serve' :: forall api context handler. 
  HasServer api context Aff handler
  => Proxy api -> Record context -> Server api -> Application
serve' p ctx s = toApplication (runRouter (const err404) (route p (Proxy :: _ Aff) ctx (emptyDelayed (Route s))))

hoistServer :: forall api ctx handler m n. 
  HasServer api ctx m handler 
  => Proxy api
  -> (forall x. m x -> n x) 
  -> Server' api m 
  -> Server' api n
hoistServer p = hoistServerWithContext p (Proxy :: Proxy ctx)

from :: forall handler api context m. HasServer api context m handler => handler -> Server' api m
from = unsafeCoerce