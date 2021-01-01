module Swerve.API.Capture where

import Data.Int as Int
import Data.Maybe (Maybe(..))

data Capture (s :: Symbol) (a :: Type) 

class ReadCapture a where 
  readCapture :: String -> Maybe a 

instance readCaptureString :: ReadCapture String where 
  readCapture x = Just x 

instance readCaptureInt :: ReadCapture Int where 
  readCapture = Int.fromString