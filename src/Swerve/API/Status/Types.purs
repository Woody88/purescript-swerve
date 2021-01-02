module Swerve.API.Status.Types where 

import Swerve.API.Types (Status')

foreign import data Continue' :: Status'
foreign import data SwitchingProtocols' :: Status'
foreign import data Ok' :: Status'
foreign import data Created' :: Status'
foreign import data Accepted' :: Status'
foreign import data NonAuthoritative' :: Status'
foreign import data NoContent' :: Status'
foreign import data ResetContent' :: Status'
foreign import data PartialContent' :: Status'
foreign import data MultipleChoices' :: Status'
foreign import data MovedPermanently' :: Status'
foreign import data Found' :: Status'
foreign import data SeeOther' :: Status'
foreign import data NotModified' :: Status'
foreign import data UseProxy' :: Status'
foreign import data TemporaryRedirect' :: Status'
foreign import data PermanentRedirect' :: Status'
foreign import data BadRequest' :: Status'
foreign import data Unauthorized' :: Status'
foreign import data PaymentRequired' :: Status'
foreign import data Forbidden' :: Status'
foreign import data NotFound' :: Status'
foreign import data MethodNotAllowed' :: Status'
foreign import data NotAcceptable' :: Status'
foreign import data ProxyAuthenticationRequired' :: Status'
foreign import data RequestTimeout' :: Status'
foreign import data Conflict' :: Status'
foreign import data Gone' :: Status'
foreign import data LengthRequired' :: Status'
foreign import data PreconditionFailed' :: Status'
foreign import data RequestEntityTooLarge' :: Status'
foreign import data RequesturiTooLong' :: Status'
foreign import data UnsupportedMediatype' :: Status'
foreign import data RequestedRangeNotSatisfiable' :: Status'
foreign import data ExpectationFailed' :: Status'
foreign import data ImaTeapot' :: Status'
foreign import data UnprocessableEntity' :: Status'
foreign import data UpgradeRequired' :: Status'
foreign import data PreconditionRequired' :: Status'
foreign import data ToomanyRequests' :: Status'
foreign import data RequestHeaderFieldsTooLarge' :: Status'
foreign import data InternalServerError' :: Status'
foreign import data NotImplemented' :: Status'
foreign import data BadGateway' :: Status'
foreign import data ServiceUnavailable' :: Status'
foreign import data GatewayTimeout' :: Status'
foreign import data HttpVersionNotSupported' :: Status'
foreign import data NetworkAuthenticationRequired' :: Status'