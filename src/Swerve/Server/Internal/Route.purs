module Swerve.Server.Internal.Route where

import Swerve.API.MediaType (kind MediaType)
import Swerve.API.RequestMethod (kind RequestMethod)

data Route (path :: Symbol) (method :: RequestMethod) body resp (ctype :: MediaType) (props :: # Type) 