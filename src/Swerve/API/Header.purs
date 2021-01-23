module Swerve.API.Header where

import Prelude 

import Data.Int as Int
import Data.Maybe (Maybe(..))

data Header (s :: Symbol) (a :: Type)

class ToHeader a where 
  toHeader :: a -> String 

instance toHeaderString :: ToHeader String where 
  toHeader a = a 

instance toHeaderInt :: ToHeader Int where 
  toHeader = show

class ReadHeader a where 
  readHeader :: String -> Maybe a 

instance readHeaderString :: ReadHeader String where 
  readHeader x = Just x 

instance readHeaderInt :: ReadHeader Int where 
  readHeader = Int.fromString

instance readHeaderMaybe :: ReadHeader a => ReadHeader (Maybe a) where 
  readHeader = readHeader