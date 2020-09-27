module Swerve.API.Header where 

import Prelude 

import Data.Int as Int
import Data.Maybe (Maybe(..))

data Header (var :: Symbol) (t :: Type)  

data Headers (row :: Row Type) (t :: Type) = Headers (Record row) t 

class ReadHeader a where 
  readHeader :: String -> Maybe a 

instance readHeaderString :: ReadHeader String where 
  readHeader x = Just x 

instance readHeaderInt :: ReadHeader Int where 
  readHeader= Int.fromString

class ToHeader a where 
  toHeader :: a -> String 

instance toHeaderString :: ToHeader String where 
  toHeader x = x 

instance toHeaderInt :: ToHeader Int where 
  toHeader = show

withHeaders :: forall hdrs t. Record hdrs -> t -> Headers hdrs t 
withHeaders = Headers 