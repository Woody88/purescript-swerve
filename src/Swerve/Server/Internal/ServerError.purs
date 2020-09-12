module Swerve.Server.Internal.ServerError where 

import Prelude

import Network.HTTP.Types (ResponseHeaders, badGateway502, badRequest400, conflict409, expectationFailed417, forbidden403, found302, gatewayTimeout504, gone410, httpVersionNotSupported505, imATeapot418, internalServerError500, lengthRequired411, methodNotAllowed405, movedPermanently301, multipleChoices300, notAcceptable406, notFound404, notImplemented501, notModified304, paymentRequired402, preconditionFailed412, proxyAuthenticationRequired407, requestEntityTooLarge413, requestURITooLong414, requestedRangeNotSatisfiable416, seeOther303, serviceUnavailable503, temporaryRedirect307, unauthorized401, unprocessableEntity422, unsupportedMediaType415, useProxy305)
import Network.Wai (Response, responseStr)

newtype ServerError 
    = ServerError
        { errStatusCode :: Int
        , errReason     :: String
        , errMessage    :: String
        , errHeaders    :: ResponseHeaders
        }

-- Needed because of constraint on ExceptT when using Alt... 
-- Need a better approach
instance semigroupServerError :: Semigroup ServerError where 
    append a b = b 

-- derive newtype instance showServerError :: Show ServerError
-- derive newtype instance eqServerError :: Eq ServerError

responseServerError :: ServerError -> Response
responseServerError (ServerError e) = responseStr status e.errHeaders e.errMessage
  where
    status = { code: e.errStatusCode, message: e.errReason }

err300 :: ServerError
err300 = ServerError 
    { errStatusCode: multipleChoices300.code 
    , errReason: multipleChoices300.message 
    , errMessage: ""
    , errHeaders: []
    }

err301 :: ServerError
err301 = ServerError 
    { errStatusCode: movedPermanently301.code 
    , errReason: movedPermanently301.message 
    , errMessage: ""
    , errHeaders: []
    }

err302 :: ServerError
err302 = ServerError 
    { errStatusCode: found302.code 
    , errReason: found302.message 
    , errMessage: ""
    , errHeaders: []
    }

err303 :: ServerError
err303 = ServerError 
    { errStatusCode: seeOther303.code 
    , errReason: seeOther303.message 
    , errMessage: ""
    , errHeaders: []
    }

err304 :: ServerError
err304 = ServerError 
    { errStatusCode: notModified304.code 
    , errReason: notModified304.message 
    , errMessage: ""
    , errHeaders: []
    }

err305 :: ServerError
err305 = ServerError 
    { errStatusCode: useProxy305.code 
    , errReason: useProxy305.message 
    , errMessage: ""
    , errHeaders: []
    }

err307 :: ServerError
err307 = ServerError 
    { errStatusCode: temporaryRedirect307.code 
    , errReason: temporaryRedirect307.message 
    , errMessage: ""
    , errHeaders: []
    }

err400 :: ServerError
err400 = ServerError 
    { errStatusCode: badRequest400.code 
    , errReason: badRequest400.message 
    , errMessage: ""
    , errHeaders: []
    }

err401 :: ServerError
err401 = ServerError 
    { errStatusCode: unauthorized401.code 
    , errReason: unauthorized401.message 
    , errMessage: ""
    , errHeaders: []
    }

err402 :: ServerError
err402 = ServerError 
    { errStatusCode: paymentRequired402.code 
    , errReason: paymentRequired402.message 
    , errMessage: ""
    , errHeaders: []
    }

err403 :: ServerError
err403 = ServerError 
    { errStatusCode: forbidden403.code 
    , errReason: forbidden403.message 
    , errMessage: ""
    , errHeaders: []
    }

err404 :: ServerError
err404 = ServerError 
    { errStatusCode: notFound404.code 
    , errReason: notFound404.message 
    , errMessage: ""
    , errHeaders: []
    }

err405 :: ServerError
err405 = ServerError 
    { errStatusCode: methodNotAllowed405.code 
    , errReason: methodNotAllowed405.message 
    , errMessage: ""
    , errHeaders: []
    }

err406 :: ServerError
err406 = ServerError 
    { errStatusCode: notAcceptable406.code 
    , errReason: notAcceptable406.message 
    , errMessage: ""
    , errHeaders: []
    }

err407 :: ServerError
err407 = ServerError 
    { errStatusCode: proxyAuthenticationRequired407.code 
    , errReason: proxyAuthenticationRequired407.message 
    , errMessage: ""
    , errHeaders: []
    }

err409 :: ServerError
err409 = ServerError 
    { errStatusCode: conflict409.code 
    , errReason: conflict409.message 
    , errMessage: ""
    , errHeaders: []
    }

err410 :: ServerError
err410 = ServerError 
    { errStatusCode: gone410.code 
    , errReason: gone410.message 
    , errMessage: ""
    , errHeaders: []
    }

err411 :: ServerError
err411 = ServerError 
    { errStatusCode: lengthRequired411.code 
    , errReason: lengthRequired411.message 
    , errMessage: ""
    , errHeaders: []
    }

err412 :: ServerError
err412 = ServerError 
    { errStatusCode: preconditionFailed412.code 
    , errReason: preconditionFailed412.message 
    , errMessage: ""
    , errHeaders: []
    }

err413 :: ServerError
err413 = ServerError 
    { errStatusCode: requestEntityTooLarge413.code 
    , errReason: requestEntityTooLarge413.message 
    , errMessage: ""
    , errHeaders: []
    }

err414 :: ServerError
err414 = ServerError 
    { errStatusCode: requestURITooLong414.code 
    , errReason: requestURITooLong414.message 
    , errMessage: ""
    , errHeaders: []
    }

err415 :: ServerError
err415 = ServerError 
    { errStatusCode: unsupportedMediaType415.code 
    , errReason: unsupportedMediaType415.message 
    , errMessage: ""
    , errHeaders: []
    }

err416 :: ServerError
err416 = ServerError 
    { errStatusCode: requestedRangeNotSatisfiable416.code 
    , errReason: requestedRangeNotSatisfiable416.message 
    , errMessage: ""
    , errHeaders: []
    }

err417 :: ServerError
err417 = ServerError 
    { errStatusCode: expectationFailed417.code 
    , errReason: expectationFailed417.message 
    , errMessage: ""
    , errHeaders: []
    }

err418 :: ServerError
err418 = ServerError 
    { errStatusCode: imATeapot418.code 
    , errReason: imATeapot418.message 
    , errMessage: ""
    , errHeaders: []
    }

err422 :: ServerError
err422 = ServerError 
    { errStatusCode: unprocessableEntity422.code 
    , errReason: unprocessableEntity422.message 
    , errMessage: ""
    , errHeaders: []
    }

err500 :: ServerError
err500 = ServerError 
    { errStatusCode: internalServerError500.code
    , errReason: internalServerError500.message
    , errMessage: ""
    , errHeaders: []
    }

err501 :: ServerError
err501 = ServerError 
    { errStatusCode: notImplemented501.code
    , errReason: notImplemented501.message
    , errMessage: ""
    , errHeaders: []
    }

err502 :: ServerError
err502 = ServerError 
    { errStatusCode: badGateway502.code
    , errReason: badGateway502.message
    , errMessage: ""
    , errHeaders: []
    }

err503 :: ServerError
err503 = ServerError 
    { errStatusCode: serviceUnavailable503.code
    , errReason: serviceUnavailable503.message
    , errMessage: ""
    , errHeaders: []
    }

err504 :: ServerError
err504 = ServerError 
    { errStatusCode: gatewayTimeout504.code
    , errReason: gatewayTimeout504.message
    , errMessage: ""
    , errHeaders: []
    }

err505 :: ServerError
err505 = ServerError 
    { errStatusCode: httpVersionNotSupported505.code
    , errReason: httpVersionNotSupported505.message
    , errMessage: ""
    , errHeaders: []
    }