module Swerve.Server where

import Network.Wai (Application)
import Prim.RowList as RL
import Swerve.Server.Internal (Server)
import Swerve.Server.Internal.Router (class RoutesHandlers, matchRoutesImpl)
import Type.Data.RowList (RLProxy(..))

swerve :: forall api apiL handlers
   . RL.RowToList api apiL
  => RoutesHandlers apiL api handlers
  => String
  -> Server api
  -> Record handlers
  -> Application
swerve path api handlers = matchRoutesImpl (RLProxy :: RLProxy apiL) api handlers 

