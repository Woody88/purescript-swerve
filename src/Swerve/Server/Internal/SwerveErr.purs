module Swerve.Server.Internal.SwerveErr where 

import Network.HTTP.Types ( ResponseHeaders, badGateway502, badRequest400, conflict409, expectationFailed417, forbidden403
                          , found302, gatewayTimeout504, gone410, httpVersionNotSupported505, imATeapot418, internalServerError500
                          , lengthRequired411, methodNotAllowed405, movedPermanently301, multipleChoices300, notAcceptable406, notFound404
                          , notImplemented501, notModified304, paymentRequired402, preconditionFailed412, proxyAuthenticationRequired407
                          , requestEntityTooLarge413, requestURITooLong414, requestedRangeNotSatisfiable416, seeOther303, serviceUnavailable503
                          , temporaryRedirect307, unauthorized401, unprocessableEntity422, unsupportedMediaType415, useProxy305)

newtype SwerveErr 
    = SwerveErr
        { errStatusCode :: Int
        , errReason     :: String
        , errMessage    :: String
        , errHeaders    :: ResponseHeaders
        }

-- derive newtype instance showSwerveErr :: Show SwerveErr
-- derive newtype instance eqSwerveErr :: Eq SwerveErr

err300 :: SwerveErr
err300 = SwerveErr 
    { errStatusCode: multipleChoices300.code 
    , errReason: multipleChoices300.message 
    , errMessage: ""
    , errHeaders: []
    }

err301 :: SwerveErr
err301 = SwerveErr 
    { errStatusCode: movedPermanently301.code 
    , errReason: movedPermanently301.message 
    , errMessage: ""
    , errHeaders: []
    }

err302 :: SwerveErr
err302 = SwerveErr 
    { errStatusCode: found302.code 
    , errReason: found302.message 
    , errMessage: ""
    , errHeaders: []
    }

err303 :: SwerveErr
err303 = SwerveErr 
    { errStatusCode: seeOther303.code 
    , errReason: seeOther303.message 
    , errMessage: ""
    , errHeaders: []
    }

err304 :: SwerveErr
err304 = SwerveErr 
    { errStatusCode: notModified304.code 
    , errReason: notModified304.message 
    , errMessage: ""
    , errHeaders: []
    }

err305 :: SwerveErr
err305 = SwerveErr 
    { errStatusCode: useProxy305.code 
    , errReason: useProxy305.message 
    , errMessage: ""
    , errHeaders: []
    }

err307 :: SwerveErr
err307 = SwerveErr 
    { errStatusCode: temporaryRedirect307.code 
    , errReason: temporaryRedirect307.message 
    , errMessage: ""
    , errHeaders: []
    }

err400 :: SwerveErr
err400 = SwerveErr 
    { errStatusCode: badRequest400.code 
    , errReason: badRequest400.message 
    , errMessage: ""
    , errHeaders: []
    }

err401 :: SwerveErr
err401 = SwerveErr 
    { errStatusCode: unauthorized401.code 
    , errReason: unauthorized401.message 
    , errMessage: ""
    , errHeaders: []
    }

err402 :: SwerveErr
err402 = SwerveErr 
    { errStatusCode: paymentRequired402.code 
    , errReason: paymentRequired402.message 
    , errMessage: ""
    , errHeaders: []
    }

err403 :: SwerveErr
err403 = SwerveErr 
    { errStatusCode: forbidden403.code 
    , errReason: forbidden403.message 
    , errMessage: ""
    , errHeaders: []
    }

err404 :: SwerveErr
err404 = SwerveErr 
    { errStatusCode: notFound404.code 
    , errReason: notFound404.message 
    , errMessage: ""
    , errHeaders: []
    }

err405 :: SwerveErr
err405 = SwerveErr 
    { errStatusCode: methodNotAllowed405.code 
    , errReason: methodNotAllowed405.message 
    , errMessage: ""
    , errHeaders: []
    }

err406 :: SwerveErr
err406 = SwerveErr 
    { errStatusCode: notAcceptable406.code 
    , errReason: notAcceptable406.message 
    , errMessage: ""
    , errHeaders: []
    }

err407 :: SwerveErr
err407 = SwerveErr 
    { errStatusCode: proxyAuthenticationRequired407.code 
    , errReason: proxyAuthenticationRequired407.message 
    , errMessage: ""
    , errHeaders: []
    }

err409 :: SwerveErr
err409 = SwerveErr 
    { errStatusCode: conflict409.code 
    , errReason: conflict409.message 
    , errMessage: ""
    , errHeaders: []
    }

err410 :: SwerveErr
err410 = SwerveErr 
    { errStatusCode: gone410.code 
    , errReason: gone410.message 
    , errMessage: ""
    , errHeaders: []
    }

err411 :: SwerveErr
err411 = SwerveErr 
    { errStatusCode: lengthRequired411.code 
    , errReason: lengthRequired411.message 
    , errMessage: ""
    , errHeaders: []
    }

err412 :: SwerveErr
err412 = SwerveErr 
    { errStatusCode: preconditionFailed412.code 
    , errReason: preconditionFailed412.message 
    , errMessage: ""
    , errHeaders: []
    }

err413 :: SwerveErr
err413 = SwerveErr 
    { errStatusCode: requestEntityTooLarge413.code 
    , errReason: requestEntityTooLarge413.message 
    , errMessage: ""
    , errHeaders: []
    }

err414 :: SwerveErr
err414 = SwerveErr 
    { errStatusCode: requestURITooLong414.code 
    , errReason: requestURITooLong414.message 
    , errMessage: ""
    , errHeaders: []
    }

err415 :: SwerveErr
err415 = SwerveErr 
    { errStatusCode: unsupportedMediaType415.code 
    , errReason: unsupportedMediaType415.message 
    , errMessage: ""
    , errHeaders: []
    }

err416 :: SwerveErr
err416 = SwerveErr 
    { errStatusCode: requestedRangeNotSatisfiable416.code 
    , errReason: requestedRangeNotSatisfiable416.message 
    , errMessage: ""
    , errHeaders: []
    }

err417 :: SwerveErr
err417 = SwerveErr 
    { errStatusCode: expectationFailed417.code 
    , errReason: expectationFailed417.message 
    , errMessage: ""
    , errHeaders: []
    }

err418 :: SwerveErr
err418 = SwerveErr 
    { errStatusCode: imATeapot418.code 
    , errReason: imATeapot418.message 
    , errMessage: ""
    , errHeaders: []
    }

err422 :: SwerveErr
err422 = SwerveErr 
    { errStatusCode: unprocessableEntity422.code 
    , errReason: unprocessableEntity422.message 
    , errMessage: ""
    , errHeaders: []
    }

err500 :: SwerveErr
err500 = SwerveErr 
    { errStatusCode: internalServerError500.code
    , errReason: internalServerError500.message
    , errMessage: ""
    , errHeaders: []
    }

err501 :: SwerveErr
err501 = SwerveErr 
    { errStatusCode: notImplemented501.code
    , errReason: notImplemented501.message
    , errMessage: ""
    , errHeaders: []
    }

err502 :: SwerveErr
err502 = SwerveErr 
    { errStatusCode: badGateway502.code
    , errReason: badGateway502.message
    , errMessage: ""
    , errHeaders: []
    }

err503 :: SwerveErr
err503 = SwerveErr 
    { errStatusCode: serviceUnavailable503.code
    , errReason: serviceUnavailable503.message
    , errMessage: ""
    , errHeaders: []
    }

err504 :: SwerveErr
err504 = SwerveErr 
    { errStatusCode: gatewayTimeout504.code
    , errReason: gatewayTimeout504.message
    , errMessage: ""
    , errHeaders: []
    }

err505 :: SwerveErr
err505 = SwerveErr 
    { errStatusCode: httpVersionNotSupported505.code
    , errReason: httpVersionNotSupported505.message
    , errMessage: ""
    , errHeaders: []
    }