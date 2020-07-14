module Swerve.API.Context where

import Data.Maybe (Maybe)
import Network.Wai.Internal (Request)
import Node.Buffer (Buffer)

newtype Context 
    = Context { request :: Request 
              , body    :: Maybe Buffer 
              }