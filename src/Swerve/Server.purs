module Swerve.Server where

import Prelude

import Effect.Aff (Aff)
import Network.Wai (Application)
import Swerve.Server.Internal (class HasServer, route)
import Type.Proxy (Proxy)

swerve :: forall layout handler m. 
  HasServer layout handler m
  => Proxy layout -> (m ~> Aff) -> handler -> Application
swerve p runH h = route p runH h 