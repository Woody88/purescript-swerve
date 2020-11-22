module Swerve.API.Status where

import Network.HTTP.Types as H
import Swerve.API.Types (Respond, Status)
import Type.Proxy (Proxy(..))


-- type Ok hdrs ctypes = Respond Ok' hdrs ctypes
-- type BadRequest hdrs ctypes = Respond BadRequest' hdrs ctypes

_Ok = Proxy :: _ Ok
_BadRequest = Proxy :: _ BadRequest
_NotFound = Proxy :: _ NotFound

foreign import data Ok :: Status
foreign import data BadRequest :: Status
foreign import data NotFound :: Status

-- data BadRequest hdrs ctype = BadRequest
-- data NotFound hdrs ctype = NotFound

class HasStatus :: forall k. k -> Constraint
class HasStatus a where 
  getStatus :: Proxy a -> H.Status

instance hasStatusOk :: HasStatus Ok where 
  getStatus _ = H.ok200

instance hasStatusBadRequest :: HasStatus BadRequest where 
  getStatus _ = H.badRequest400

instance hasStatusNotFound :: HasStatus NotFound where 
  getStatus _ = H.notFound404