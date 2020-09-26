module Swerve.API.Raw where

import Prelude

import Swerve.API.Combinators (type (:>))
import Swerve.API.Resource (Resource)

type Raw path = Raw' path :> Resource Unit Unit 

data Raw' (path :: Symbol)
