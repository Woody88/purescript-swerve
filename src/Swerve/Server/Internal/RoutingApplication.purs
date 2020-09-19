module Swerve.Server.Internal.RoutingApplication where

import Prelude

import Effect.Aff (Aff)
import Network.HTTP.Types (notFound404)
import Network.Wai (Application, Request, Response, responseStr)
import Swerve.Server.Internal.RouteResult (RouteResult(..))

-- import Type.Data.Row (RProxy)
-- import Type.Data.RowList (RLProxy)
-- import Type.Equality (class TypeEquals)
-- import Type.Equality as TypeEq
-- import Type.Proxy (Proxy(..))

type RoutingApplication a = Request -> (RouteResult a -> Aff Unit) -> Aff Unit

toApplication :: RoutingApplication Response -> Application
toApplication ra request respond = ra request routingRespond
  where
    routingRespond :: RouteResult Response -> Aff Unit
    routingRespond NotMatched   = respond $ responseStr notFound404 [] ""
    routingRespond (Matched v)    = respond v