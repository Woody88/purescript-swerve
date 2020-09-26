module Swerve.Server.Internal.HasConn where

import Prim.Row as Row
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.Header (Header)
import Swerve.API.Query (Query)
import Swerve.API.Resource (Resource)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class HasConn (api :: Type) (from :: Row Type) (to :: Row Type) | api -> from to

instance hasConnResource :: HasConn (Resource a ctype) to to  

instance hasConnCapture :: 
  ( CaptureAccum (Capture vname t :> api) () to
  , Row.Cons "capture" (Record to) from from' 
  , HasConn api from' to_ 
  , Row.Union from' to_ to' 
  , Row.Nub to' to__ 
  ) => HasConn (Capture vname t :> api) from to__

instance hasConnQuery :: 
  ( QueryAccum (Query vname t :> api) () to
  , Row.Cons "query" (Record to) from from' 
  , HasConn api from' to_ 
  , Row.Union from' to_ to' 
  , Row.Nub to' to__ 
  ) => HasConn (Query vname t :> api) from to__ 

instance hasConnHeader :: 
  ( HeaderAccum (Header vname t :> api) () to
  , Row.Cons "header" (Record to) from from' 
  , HasConn api from' to_ 
  , Row.Union from' to_ to' 
  , Row.Nub to' to__ 
  ) => HasConn (Header vname t :> api) from to__ 

-- | Accumulates capture conn in a row type.
class CaptureAccum (api :: Type) (from :: Row Type) (to :: Row Type) | api -> from to

instance accumCapture :: 
  ( Row.Cons vname t from from' 
  , CaptureAccum api from' to 
  ) => CaptureAccum (Capture vname t :> api) from to

else instance accumCapBase :: CaptureAccum api to to 

-- | Accumulates query conn in a row type.
class QueryAccum (api :: Type) (from :: Row Type) (to :: Row Type) | api -> from to

instance accumQuery :: 
  ( Row.Cons vname t from from' 
  , QueryAccum api from' to 
  ) => QueryAccum (Query vname t :> api) from to

else instance accumQueryBase :: QueryAccum api to to 

-- | Accumulates header conn in a row type.
class HeaderAccum (api :: Type) (from :: Row Type) (to :: Row Type) | api -> from to

instance accumHeader :: 
  ( Row.Cons vname t from from' 
  , HeaderAccum api from' to 
  ) => HeaderAccum (Header vname t :> api) from to

else instance accumHeaderBase :: HeaderAccum api to to 

hasConn :: forall api to. 
  HasConn api () to 
  => Proxy api 
  -> Proxy to
hasConn _ = Proxy 