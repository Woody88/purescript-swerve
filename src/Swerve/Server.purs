module Swerve.Server where

import Effect (Effect)
import Swerve.Server.Internal (class HasServer, route)
import Type.Proxy (Proxy)

-- import Network.Wai (Application)
-- import Prim.RowList as RL
-- import Swerve.Server.Internal (Server, class RoutesHandlers, matchRoutesImpl)
-- import Swerve.Server.Internal.RouterApplication (toApplication)
-- import Type.Data.Row (RProxy(..))
-- import Type.Data.RowList (RLProxy(..))

-- swerve :: forall api apiL handlers
--    . RL.RowToList api apiL
--   => RoutesHandlers apiL api handlers
--   => Server api
--   -> Record handlers
--   -> Application
-- swerve api handlers = toApplication $ matchRoutesImpl (RLProxy :: RLProxy apiL) (RProxy :: RProxy api) handlers 

swerve :: forall layout handler. 
  HasServer layout handler 
  => Proxy layout -> handler -> String -> Effect String
swerve p h xs = route p h xs
  
