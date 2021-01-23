module Swerve.API.Capture where

import Prelude 

import Data.Int as Int
import Data.Maybe (Maybe(..))

data Capture (s :: Symbol) (a :: Type) 

class ToCapture a where 
  toCapture :: a -> String 

instance toCaptureString :: ToCapture String where 
  toCapture a = a 

instance toCaptureInt :: ToCapture Int where 
  toCapture = show

class ReadCapture a where 
  readCapture :: String -> Maybe a 

instance readCaptureString :: ReadCapture String where 
  readCapture x = Just x 

instance readCaptureInt :: ReadCapture Int where 
  readCapture = Int.fromString