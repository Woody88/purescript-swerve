module Swerve.Server.Internal.RoutingApplication where

import Prelude

import Effect.Aff (Aff)
import Network.HTTP.Types (notFound404)
import Network.Wai (Application, Request, Response, responseStr)

data RouteResult a 
  = Matched a 
  | NotMatched 
  | RawRoute Application
  | Choice (RoutingApplication a) (RoutingApplication a)

type RoutingApplication a = Request -> (RouteResult a -> Aff Unit) -> Aff Unit

toApplication :: RoutingApplication Response -> Application
toApplication ra request respond = ra request routingRespond
  where
    routingRespond :: RouteResult Response -> Aff Unit
    routingRespond NotMatched     = respond $ responseStr notFound404 [] ""
    routingRespond (Matched v)    = respond v
    routingRespond (RawRoute app) = app request respond
    routingRespond (Choice app app2) = app request (flip routeResult app2)

    routeResult NotMatched app = app request routingRespond
    routeResult rr app = routingRespond rr 