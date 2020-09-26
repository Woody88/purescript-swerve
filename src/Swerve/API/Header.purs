module Swerve.API.Header where 

import Data.Int as Int
import Data.Maybe (Maybe(..))

data Header (var :: Symbol) (t :: Type)  

class ReadHeader a where 
  readHeader :: String -> Maybe a 

instance readHeaderString :: ReadHeader String where 
  readHeader x = Just x 

instance readHeaderInt :: ReadHeader Int where 
  readHeader= Int.fromString