module Swerve.Server.ServerError where

import Network.HTTP.Types as H

type ServerError 
  = { content :: String 
    , headers :: H.ResponseHeaders
    , status  :: H.Status
    }