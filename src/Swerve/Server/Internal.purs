module Swerve.Server.Internal where

import Network.Wai (Response)
import Swerve.Server.Internal.RoutingApplication (RoutingApplication)
import Type.Proxy (Proxy)


class HasServer :: forall k. k -> Type -> Constraint
class HasServer api handler where 
  route :: Proxy api -> handler -> RoutingApplication Response

-- import Prelude

-- import Control.Monad.Except (class MonadError, runExceptT)
-- import Data.Either (Either(..))
-- import Effect.Aff (Aff)
-- import Network.HTTP.Types (notFound404)
-- import Network.Wai (Application, responseStr)
-- import Swerve.Server.Internal.Response (SwerveResponse(..))
-- import Swerve.Server.Internal.RouterI (class RouterI, routerI)
-- import Swerve.Server.Internal.ServerError (ServerError(..), responseServerError)
-- import Type.Proxy (Proxy)

-- class HasServer api handler m where 
--   route :: Proxy api -> (forall a. m a -> Aff (Either ServerError a)) -> handler -> Application 

-- instance hasServerAlt :: 
--   ( RouterI api handler m
--   , Monad m
--   , MonadError ServerError m
--   ) => HasServer api handler m where 
--   route api runM handler req resp = do 
--     routerResult <- runExceptT $ routerI api handler req
--     case routerResult of 
--       Left (ServerError e) -> resp $ responseStr notFound404 [] e.errMessage
--       Right handlerM -> do 
--         result <- runM handlerM
--         case result of
--           Left e   -> resp $ responseServerError e
--           Right rsp -> case rsp of 
--             Rsp response -> resp response
--             Raw app      -> app req resp