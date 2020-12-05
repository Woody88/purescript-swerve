module Swerve.Server 
  ( module Response
  -- , module ServerType
  , module BasicAuth 
  , serve
  , serveWithContext 
  , hoistServer
  , from 
  ) where

import Prelude 

import Effect.Aff (Aff)
import Network.Wai (Application) 
import Swerve.Server.Internal.ServerContext (Server, Server')
-- import Swerve.Server.Internal (Server, Server') as ServerType
import Swerve.Server.Internal (class HasServer, hoistServerWithContext, route)
import Swerve.Server.Internal.BasicAuth (BasicAuthCheck(..), BasicAuthResult(..)) as BasicAuth
import Swerve.Server.Internal.Delayed (emptyDelayed)
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Swerve.Server.Internal.Router (runRouter)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.ServerError (err404)
import Swerve.Server.Internal.Response (class HasResp, Response(..), raise, respond) as Response
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

serve :: forall api handler. 
  HasServer Server' api () Aff handler
  => Proxy api -> Server api -> Application
serve p s = toApplication (runRouter (const err404) (route (Proxy :: _ Server') p (Proxy :: _ Aff) {} (emptyDelayed (Route s))))

serveWithContext :: forall api context handler. 
  HasServer Server' api context Aff handler
  => Proxy api -> Record context -> Server api -> Application
serveWithContext p ctx s = toApplication (runRouter (const err404) (route (Proxy :: _ Server') p (Proxy :: _ Aff) ctx (emptyDelayed (Route s))))

hoistServer :: forall api ctx handler m n. 
  HasServer Server' api ctx m handler 
  => Proxy api
  -> (forall x. m x -> n x) 
  -> Server' api m 
  -> Server' api n
hoistServer p = hoistServerWithContext (Proxy :: _ Server') p (Proxy :: Proxy ctx)

from :: forall handler api context m. HasServer Server' api context m handler => handler -> Server' api m
from = unsafeCoerce