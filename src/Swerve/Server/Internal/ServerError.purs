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

err400 :: ServerError
err400 = { content: ""
         , headers: []
         , status: H.badRequest400
         }

err415 :: ServerError
err415 = { content: ""
         , headers: []
         , status: H.unsupportedMediaType415
         }

err500 :: ServerError
err500 = { content: ""
         , headers: []
         , status: H.internalServerError500
         }