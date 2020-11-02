module Swerve.API.QueryParam where

import Data.Int as Int
import Data.Maybe (Maybe(..))

class ReadQuery a where 
  readQuery :: String -> Maybe a 

instance readQueryString :: ReadQuery String where 
  readQuery x = Just x 

instance readQueryInt :: ReadQuery Int where 
  readQuery = Int.fromString

instance readQueryMaybe :: ReadQuery a => ReadQuery (Maybe a) where 
  readQuery = readQuery 