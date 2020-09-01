module Swerve.API.Context where

import Data.Maybe (Maybe)
import Network.Wai (Request)
import Node.Buffer (Buffer)

newtype Context 
    = Context { request :: Request 
              , body    :: Maybe Buffer 
              }