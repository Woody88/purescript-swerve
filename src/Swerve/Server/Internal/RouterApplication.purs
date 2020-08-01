module Swerve.Server.Internal.RouterApplication where

-- import Prelude

-- import Data.Either (Either(..))
-- import Data.Symbol (class IsSymbol, SProxy(..))
-- import Effect (Effect)
-- import Network.Wai (Application, Request(..), Response)
-- import Prim.Row as Row
-- import Prim.RowList as RL
-- import Record as Record
-- import Swerve.API.RequestMethod (GetRequest)
-- import Swerve.Server.Internal.Handler (Handler)
-- import Swerve.Server.Internal.ParseCapture (class ParseCapture, parseCapture)
-- import Swerve.Server.Internal.Response (class HasResponse, err400Response, toResponse)
-- import Swerve.Server.Internal.Route (Route, RouteResult(..))
-- import Type.Data.Row (RProxy)
-- import Type.Data.RowList (RLProxy)
-- import Type.Equality (class TypeEquals)
-- import Type.Equality as TypeEq
-- import Type.Proxy (Proxy(..))

-- type RoutingApplication = Request -> (RouteResult Response -> Effect Unit) -> Effect Unit

-- toApplication :: RoutingApplication -> Application
-- toApplication ra request respond = ra request routingRespond
--   where
--     routingRespond :: RouteResult Response -> Effect Unit
--     routingRespond (NotMatched)    = respond $ err400Response
--     routingRespond (Matched v)     = respond v