module Swerve.Server.Internal.HasConn where

import Prim.Row as Row
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.Resource (Resource)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

-- | Will generate a connection row type for a given api 
class HasConn (api :: Type) (to :: Row Type) | api -> to

instance hasConnResource :: HasConn (Resource a ctype) () 

instance hasConnCapture :: 
  ( CaptureConn (Capture vname t :> api) () to 
  , TypeEquals (Record ()) (Record to')
  , Row.Cons "capture" (Record to) to' to_
  ) => HasConn (Capture vname t :> api) to_

-- | Accumulates all captures in a row type.
class CaptureConn (api :: Type) (from :: Row Type) (to :: Row Type) | api -> from to

instance captureConnBase :: CaptureConn (Resource a ctype) to to 

instance captureConn :: 
  (CaptureConn api from from', Row.Cons vname t from' to) => CaptureConn (Capture vname t :> api) from to 

hasConn :: forall api to. 
  HasConn api to 
  => Proxy api 
  -> Proxy to
hasConn _ = Proxy 