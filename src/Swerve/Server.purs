module Swerve.Server where

import Prelude 

import Network.Wai (Application)
import Swerve.Server.Internal (class HasServer, route)
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Type.Proxy (Proxy(..))

-- import Control.Monad.Except (ExceptT, runExceptT)
-- import Data.Either (Either)
-- import Effect.Aff (Aff)
-- import Network.Wai (Application)
-- import Swerve.Server.Internal (class HasServer, route)
-- import Swerve.Server.Internal.ServerError (ServerError)
-- import Type.Proxy (Proxy)

swerve :: forall api handler. 
  HasServer api () handler 
  => Proxy api 
  -> handler 
  -> Application
swerve p h = swerveContext p {} h

swerveContext :: forall api ctx handler. 
  HasServer api ctx handler 
  => Proxy api 
  -> Record ctx 
  -> handler 
  -> Application
swerveContext p ctx h = toApplication $ route p ctx h


-- swerve :: forall layout handler. 
--   HasServer layout handler (ExceptT ServerError Aff)
--   => Proxy layout 
--   -> handler 
--   -> Application
-- swerve p h = swerveHoist p runExceptT h

-- swerveHoist :: forall layout handler m. 
--   HasServer layout handler m
--   => Proxy layout 
--   -> (forall a. m a -> Aff (Either ServerError a))
--   -> handler 
--   -> Application
-- swerveHoist p runH h = route p runH h 