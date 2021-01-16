module Swerve.Server.Internal.RoutingApplication where

import Prelude

import Effect.Aff (Aff)
import Network.Wai (Application, Request, Response, ResponseReceived)
import Swerve.Server.Internal.ServerError (responseServerError)
import Swerve.Server.Internal.RouteResult (RouteResult(..))

type RoutingApplication = Request -> (RouteResult Response -> Aff ResponseReceived) -> Aff ResponseReceived

toApplication :: RoutingApplication -> Application
toApplication ra request respond = ra request routingRespond
 where
  routingRespond :: RouteResult Response -> Aff ResponseReceived
  routingRespond (Fail err)      = respond $ responseServerError err
  routingRespond (FailFatal err) = respond $ responseServerError err
  routingRespond (Route v)       = respond v