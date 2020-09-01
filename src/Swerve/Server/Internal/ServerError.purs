module Swerve.Server.Internal.ServerError where 

import Network.HTTP.Types as HTTP

newtype ServerError 
    = ServerError
        { errHTTPCode     :: Int
        , errReasonPhrase :: String
        , errBody         :: String
        , errHeaders      :: Array HTTP.Header
        }

-- derive newtype instance showServerError :: Show ServerError
-- derive newtype instance eqServerError :: Eq ServerError

err500 :: ServerError
err500 = ServerError { errHTTPCode: 500
                     , errReasonPhrase: "Internal Server Error"
                     , errBody: ""
                     , errHeaders: []
                     }
