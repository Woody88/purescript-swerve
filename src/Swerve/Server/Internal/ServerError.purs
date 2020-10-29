module Swerve.Server.Internal.ServerError where

import Network.HTTP.Types as H
import Network.Wai (Response, responseStr)

type ServerError 
  = { content :: String 
    , headers :: H.ResponseHeaders
    , status  :: H.Status
    }

responseServerError :: ServerError -> Response
responseServerError se = responseStr se.status se.headers se.content

err404 :: ServerError
err404 = { content: ""
         , headers: []
         , status: H.notFound404
         }
