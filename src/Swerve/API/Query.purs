module Swerve.API.Query where 

import Data.Int as Int
import Data.Maybe (Maybe(..))

data Query (var :: Symbol) (t :: Type)  

class ReadQuery a where 
  readQuery :: String -> Maybe a 

instance readQueryString :: ReadQuery String where 
  readQuery x = Just x 

instance readQueryInt :: ReadQuery Int where 
  readQuery = Int.fromString