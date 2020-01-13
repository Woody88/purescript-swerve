module Swerve.Server.Internal.RoutinApplication where

import Prelude

import Effect (Effect)
import Network.Wai.Internal (Request, Response)
import Swerve.Server.Internal.RouterResult (RouteResult)

type RoutingApplication = Request -> (RouteResult Response -> Effect Unit) -> Effect Unit