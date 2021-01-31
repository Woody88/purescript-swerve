module Swerve.Client.Internal.BasicAuth where 

import Prelude 

import Data.Newtype (wrap)
import Data.String.Base64 as Base64 
import Swerve.API.BasicAuth (BasicAuthData(..))
import Swerve.Client.Internal.Request (Request, addHeader)

-- | Authenticate a request using Basic Authentication
basicAuthReq :: BasicAuthData -> Request -> Request
basicAuthReq (BasicAuthData b) req = let 
  auth = "Basic " <> (Base64.encode (b.username <> ":" <> b.password))
  in addHeader (wrap "Authorization") auth req