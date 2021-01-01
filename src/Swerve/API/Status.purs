module Swerve.API.Status where

import Network.HTTP.Types as H
import Swerve.API.Types (Status)
import Type.Proxy (Proxy(..))

_Ok = Proxy :: _ Ok
_BadRequest = Proxy :: _ BadRequest
_NotFound = Proxy :: _ NotFound
_Forbidden = Proxy :: _ Forbidden

foreign import data Ok :: Status
foreign import data Forbidden :: Status
foreign import data BadRequest :: Status
foreign import data NotFound :: Status

foreign import data NoContent' :: Status 
foreign import data Ok' :: Status
foreign import data Forbidden' :: Status
foreign import data BadRequest' :: Status

class HasStatus' a label | a -> label where 
  statusOf :: Proxy a -> H.Status 

instance hasStatusNoContent' :: HasStatus' NoContent' "204" where 
  statusOf _ = H.noContent204

instance hasStatusOk' :: HasStatus' Ok' "200" where 
  statusOf _ = H.ok200

instance hasStatusBadRequest' :: HasStatus' BadRequest' "400" where 
  statusOf _ = H.badRequest400

instance hasStatusWithStatus :: HasStatus' status label => HasStatus' (WithStatus status a) label where 
  statusOf _ = statusOf (Proxy :: _ status)

data WithStatus (k :: Status) a = WithStatus (Proxy k) a

class HasStatus :: forall k. k -> Symbol -> Constraint
class HasStatus a label | a -> label where 
  getStatus :: Proxy a -> H.Status

instance hasStatusOk :: HasStatus Ok "ok" where 
  getStatus _ = H.ok200

instance hasStatusBadRequest :: HasStatus BadRequest "badRequest" where 
  getStatus _ = H.badRequest400

instance hasStatusNotFound :: HasStatus NotFound "notFound" where 
  getStatus _ = H.notFound404

instance hasStatusForbidden :: HasStatus Forbidden "forbidden" where 
  getStatus _ = H.forbidden403