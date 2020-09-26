module Swerve.Utils where

import Prelude

import Data.String (Pattern(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))

queryInfo :: String -> Array (Tuple String String)
queryInfo q = 
  q
  # String.split (Pattern "&")
  # map (splitAt (flip Tuple "") "=")
  where 
    splitAt k p str =
        case String.indexOf (Pattern p) str of
          Just i -> Tuple (String.take i str) (String.drop (i + String.length p) str)
          Nothing -> k str