module Swerve.Server.Internal.Method where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either)
import Data.Newtype (unwrap)
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Method (methodGet, methodHead)
import Network.Wai (Request)

allowedMethodHead :: Method -> Request -> Boolean
allowedMethodHead method request = method == methodGet && (show $ _.method $ unwrap request) == methodHead

allowedMethod :: Method -> Request -> Boolean
allowedMethod method request = allowedMethodHead method request || (show $ _.method $ unwrap request) == method

methodCheck :: Method -> Request -> Either String Unit 
methodCheck method request
  | allowedMethod method request = pure unit 
  | otherwise                    = throwError "Method not allowed"
