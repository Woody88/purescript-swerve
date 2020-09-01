module Swerve.Server.Internal.RouterApplication where

import Prelude

import Effect.Aff (Aff)
import Network.Wai (Request, Response)

-- import Type.Data.Row (RProxy)
-- import Type.Data.RowList (RLProxy)
-- import Type.Equality (class TypeEquals)
-- import Type.Equality as TypeEq
-- import Type.Proxy (Proxy(..))

-- type RoutingApplication = Request -> (RouteResult Response -> Aff Unit) -> Aff Unit

-- toApplication :: RoutingApplication -> Application
-- toApplication ra request respond = ra request routingRespond
--   where
--     routingRespond :: RouteResult Response -> Effect Unit
--     routingRespond (NotMatched)    = respond $ err400Response
--     routingRespond (Matched v)     = respond v