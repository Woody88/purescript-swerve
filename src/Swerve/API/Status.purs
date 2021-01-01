module Swerve.API.Status where

import Network.HTTP.Types as H
import Swerve.API.Types (Status')
import Type.Proxy (Proxy(..))

foreign import data NoContent' :: Status' 
foreign import data Ok' :: Status'
foreign import data Forbidden' :: Status'
foreign import data BadRequest' :: Status'

data WithStatus (k :: Status') a = WithStatus (Proxy k) a

class HasStatus :: forall k. k -> Symbol -> Constraint
class HasStatus a label | a -> label where 
  statusOf :: Proxy a -> H.Status 

instance hasStatus204 :: HasStatus NoContent' "204" where 
  statusOf _ = H.noContent204

instance hasStatus200 :: HasStatus Ok' "200" where 
  statusOf _ = H.ok200

instance hasStatus400 :: HasStatus BadRequest' "400" where 
  statusOf _ = H.badRequest400

instance hasStatus403 :: HasStatus Forbidden' "403" where 
  statusOf _ = H.forbidden403

instance hasStatusWithStatus :: HasStatus status label => HasStatus (WithStatus status a) label where 
  statusOf _ = statusOf (Proxy :: _ status)
