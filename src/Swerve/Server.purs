module Swerve.Server where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Network.Wai (Application)
import Swerve.Server.Internal (class HasServer, route)
import Type.Proxy (Proxy)

swerve :: forall layout handler. 
  HasServer layout handler (ExceptT String Aff)
  => Proxy layout 
  -> handler 
  -> Application
swerve p h = swerveHoist p runExceptT h

swerveHoist :: forall layout handler m. 
  HasServer layout handler m
  => Proxy layout 
  -> (forall a. m a -> Aff (Either String a))
  -> handler 
  -> Application
swerveHoist p runH h = route p runH h 