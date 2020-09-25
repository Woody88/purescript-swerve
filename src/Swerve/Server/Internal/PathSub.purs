module Swerve.Server.InternalPathSub where

import Swerve.API.Combinators (type (:>))
import Swerve.Server.Internal.Path (CaptureVar, PCons, PList, PNil, Segment)

-- | Extract path types and appends it to api 
class PathSub (plist :: PList) (api :: Type) (api' :: Type) | plist api -> api' 

instance pathSubPNil :: PathSub PNil api api 

instance pathSubCapture :: PathSub rest api b => PathSub (PCons (CaptureVar sym) rest) api (CaptureVar sym :> b)

instance pathToSubSegmentRoot :: PathSub rest api b => PathSub (PCons (Segment "") rest) api (Segment "/" :> b)
else instance pathToSubSegment :: PathSub rest api b => PathSub (PCons (Segment sym) rest) api (Segment sym :> b)