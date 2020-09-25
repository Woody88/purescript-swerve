module Swerve.Server.InternalPathSub where

import Swerve.API.Combinators (type (:>))
import Swerve.Server.Internal.Path (CaptureVar, PCons, PList, PNil, Segment)

-- | Extract path types and appends it to api 
class PathSub (plist :: PList) (api :: Type) | plist -> api

data PathEnd 

instance pathSubPNil :: PathSub PNil PathEnd

instance pathSubCapture :: PathSub rest b => PathSub (PCons (CaptureVar sym) rest) (CaptureVar sym :> b)

instance pathToSubSegmentRoot :: PathSub rest b => PathSub (PCons (Segment "") rest) (Segment "/" :> b)
else instance pathToSubSegment :: PathSub rest b => PathSub (PCons (Segment sym) rest) (Segment sym :> b)