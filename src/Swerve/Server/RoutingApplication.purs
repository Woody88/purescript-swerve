module Swerve.Server.RoutingApplication where

import Prelude

import Effect.Aff (Aff)
import Network.Wai (Application, Request, Response)
import Swerve.Server.RouteResult (RouteResult(..))
import Swerve.Server.ServerError (responseServerError)

type RoutingApplication = Request -> (RouteResult Response -> Aff Unit) -> Aff Unit

toApplication :: RoutingApplication -> Application
toApplication ra request respond = ra request routingRespond
 where
  routingRespond :: RouteResult Response -> Aff Unit
  routingRespond (Fail err)      = respond $ responseServerError err
  routingRespond (FailFatal err) = respond $ responseServerError err
  routingRespond (Route v)       = respond v