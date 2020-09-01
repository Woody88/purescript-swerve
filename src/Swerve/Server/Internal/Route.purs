module Swerve.Server.Internal.Route where

import Swerve.Server.Internal.ServerError (ServerError(..))

-- import Prelude

-- import Swerve.API.MediaType (kind MediaType)
-- import Swerve.API.RequestMethod (GetRequest, kind RequestMethod)

-- data Route (path :: Symbol) (method :: RequestMethod) body resp (ctype :: MediaType) (specs :: # Type) 

-- type Get url res ctype props = Route url GetRequest Void res ctype props 

-- type Post url res body ctype props = Route url GetRequest body res ctype props 

data RouteResult a = NotMatched | Matched a
