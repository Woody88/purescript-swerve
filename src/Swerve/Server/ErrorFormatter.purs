module Swerve.Server.ErrorFormatter where 

import Network.Wai (Request)
import Swerve.Server.ServerError (ServerError)

type NotFoundErrorFormatter = Request -> ServerError
