module Swerve.Server where

import Prelude 

import Network.Wai (Application)
import Prim.RowList as RL
import Swerve.Server.Internal (Server, class RoutesHandlers, matchRoutesImpl)
import Swerve.Server.Internal.RouterApplication (toApplication)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))

swerve :: forall api apiL handlers
   . RL.RowToList api apiL
  => RoutesHandlers apiL api handlers
  => Server api
  -> Record handlers
  -> Application
swerve api handlers = toApplication $ matchRoutesImpl (RLProxy :: RLProxy apiL) (RProxy :: RProxy api) handlers 

