module Swerve.API.Status where

import Network.HTTP.Types as H
import Swerve.API.Types (Status)
import Type.Proxy (Proxy)

foreign import data Ok :: Status

class HasStatus :: forall k. k -> Symbol -> Constraint
class HasStatus a (label :: Symbol) | a -> label, label -> a where 
  getStatus :: Proxy a -> H.Status

instance hasStatusOk :: HasStatus Ok "ok" where 
  getStatus _ = H.ok200