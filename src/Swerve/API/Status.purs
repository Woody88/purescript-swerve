module Swerve.API.Status where

import Network.HTTP.Types as H
import Swerve.API.Types (Respond, Status)
import Type.Proxy (Proxy)


type Ok hdrs ctypes = Respond Ok' hdrs ctypes
type BadRequest hdrs ctypes = Respond BadRequest' hdrs ctypes

foreign import data Ok' :: Status
foreign import data BadRequest' :: Status

-- data BadRequest hdrs ctype = BadRequest
-- data NotFound hdrs ctype = NotFound

class HasStatus :: forall k. k -> Symbol -> Constraint
class HasStatus a (label :: Symbol) | a -> label, label -> a where 
  getStatus :: Proxy a -> H.Status

instance hasStatusOk :: HasStatus Ok' "ok" where 
  getStatus _ = H.ok200

instance hasStatusBadRequest :: HasStatus BadRequest' "badRequest" where 
  getStatus _ = H.badRequest400

-- instance hasStatusNotFound :: HasStatus NotFound "notFound" where 
--   getStatus _ = H.notFound404