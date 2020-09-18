module Swerve.Server.Internal.RoutingApplication where

import Prelude

import Effect.Aff (Aff)
import Network.Wai (Request)
import Swerve.Server.Internal.RouteResult (RouteResult)

-- import Type.Data.Row (RProxy)
-- import Type.Data.RowList (RLProxy)
-- import Type.Equality (class TypeEquals)
-- import Type.Equality as TypeEq
-- import Type.Proxy (Proxy(..))

type RoutingApplication a = Request -> (RouteResult a -> Aff Unit) -> Aff Unit

-- toApplication :: RoutingApplication -> Application
-- toApplication ra request respond = ra request routingRespond
--   where
--     routingRespond :: RouteResult Response -> Effect Unit
--     routingRespond (NotMatched)    = respond $ err400Response
--     routingRespond (Matched v)     = respond v