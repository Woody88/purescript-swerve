module Swerve.Server where

import Network.Wai (Application)
import Swerve.Server.Internal (class HasServer, route)
import Type.Proxy (Proxy)

swerve :: forall layout handler. 
  HasServer layout handler 
  => Proxy layout -> handler -> Application
swerve p h xs = route p h xs