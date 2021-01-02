module Swerve.API.Status where

import Swerve.API.Status.KnownStatus (class KnownStatus)
import Swerve.API.Status.Types 
import Swerve.API.Types (Status')
import Type.Proxy (Proxy)

data WithStatus (k :: Status') a = WithStatus (Proxy k) a

class HasStatus :: forall k. k -> Symbol -> Constraint
class KnownStatus statusNat <= HasStatus a statusNat | a -> statusNat  

instance hasStatusWithStatus :: HasStatus status statusNat => HasStatus (WithStatus status a) statusNat 
instance hasStatusContinue                       :: HasStatus Continue' "100" 
instance hasStatusSwitchingProtocols             :: HasStatus SwitchingProtocols' "101"
instance hasStatusOk                             :: HasStatus Ok' "200"
instance hasStatusCreated                        :: HasStatus Created' "201"
instance hasStatusAccepted                       :: HasStatus Accepted' "202"
instance hasStatusNonAuthoritative               :: HasStatus NonAuthoritative' "203"
instance hasStatusNoContent                      :: HasStatus NoContent' "204"
instance hasStatusResetContent                   :: HasStatus ResetContent' "205"
instance hasStatusPartialContent                 :: HasStatus PartialContent' "206"
instance hasStatusMultipleChoices                :: HasStatus MultipleChoices' "300"
instance hasStatusMovedPermanently               :: HasStatus MovedPermanently' "301"
instance hasStatusFound                          :: HasStatus Found' "302"
instance hasStatusSeeOther                       :: HasStatus SeeOther' "303"
instance hasStatusNotModified                     :: HasStatus NotModified' "304"
instance hasStatusUseProxy                       :: HasStatus UseProxy' "305"
instance hasStatusTemporaryRedirect              :: HasStatus TemporaryRedirect' "307"
instance hasStatusPermanentRedirect              :: HasStatus PermanentRedirect' "308"
instance hasStatusBadRequest                     :: HasStatus BadRequest' "400"
instance hasStatusUnauthorized                   :: HasStatus Unauthorized' "401"
instance hasStatusPaymentRequired                :: HasStatus PaymentRequired' "402"
instance hasStatusForbidden                      :: HasStatus Forbidden' "403"
instance hasStatusNotFound                       :: HasStatus NotFound' "404"
instance hasStatusMethodNotAllowed               :: HasStatus MethodNotAllowed' "405"
instance hasStatusNotAcceptable                  :: HasStatus NotAcceptable' "406"
instance hasStatusProxyAuthenticationRequired    :: HasStatus ProxyAuthenticationRequired' "407"
instance hasStatusRequestTimeout                 :: HasStatus RequestTimeout' "408"
instance hasStatusConflict                        :: HasStatus Conflict' "409"
instance hasStatusGone                           :: HasStatus Gone' "410"
instance hasStatusLengthRequired                 :: HasStatus LengthRequired' "411"
instance hasStatusPreconditionFailed             :: HasStatus PreconditionFailed' "412"
instance hasStatusRequestEntityTooLarge          :: HasStatus RequestEntityTooLarge' "413"
instance hasStatusRequesturiTooLong              :: HasStatus RequesturiTooLong' "414"
instance hasStatusUnsupportedMediatype           :: HasStatus UnsupportedMediatype' "415"
instance hasStatusRequestedRangeNotSatisfiable    :: HasStatus RequestedRangeNotSatisfiable' "416"
instance hasStatusExpectationFailed              :: HasStatus ExpectationFailed' "417"
instance hasStatusImaTeapot                      :: HasStatus ImaTeapot' "418"
instance hasStatusUnprocessableEntity            :: HasStatus UnprocessableEntity' "422"
instance hasStatusUpgradeRequired                :: HasStatus UpgradeRequired' "426"
instance hasStatusPreconditionRequired           :: HasStatus PreconditionRequired' "428"
instance hasStatusToomanyRequests                :: HasStatus ToomanyRequests' "429"
instance hasStatusRequestHeaderFieldsTooLarge    :: HasStatus RequestHeaderFieldsTooLarge' "431"
instance hasStatusInternalServerError            :: HasStatus InternalServerError' "500"
instance hasStatusNotImplemented                 :: HasStatus NotImplemented' "501"
instance hasStatusBadGateway                     :: HasStatus BadGateway' "502"
instance hasStatusServiceUnavailable             :: HasStatus ServiceUnavailable' "503"
instance hasStatusGatewayTimeout                 :: HasStatus GatewayTimeout' "504"
instance hasStatusHttpVersionNotSupported        :: HasStatus HttpVersionNotSupported' "505"
instance hasStatusNetworkAuthenticationRequired  :: HasStatus NetworkAuthenticationRequired' "511"
