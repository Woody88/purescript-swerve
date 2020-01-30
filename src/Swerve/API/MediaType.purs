module Swerve.API.MediaType where 

import Data.MediaType (MediaType) as M

newtype MediaType = MediaType M.MediaType

foreign import kind MediaType 

foreign import data JSON :: MediaType
data JSON'