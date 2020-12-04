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

err400 :: ServerError
err400 = { content: ""
         , headers: []
         , status: H.badRequest400
         }

err401 :: ServerError
err401 = { content: ""
         , headers: []
         , status: H.unauthorized401
         }

err403 :: ServerError
err403 = { content: ""
         , headers: []
         , status: H.forbidden403
         }

err404 :: ServerError
err404 = { content: ""
         , headers: []
         , status: H.notFound404
         }

err405 :: ServerError
err405 = { content: ""
         , headers: []
         , status: H.methodNotAllowed405
         }

err406 :: ServerError
err406 = { content: ""
         , headers: []
         , status: H.notAcceptable406
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