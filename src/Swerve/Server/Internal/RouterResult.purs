module Swerve.Server.Internal.RouterResult where

import Swerve.Server.Internal.ServerError (ServerError)

data RouteResult a 
    = Fail ServerError                 
    | FailFatal ServerError   
    | Route a