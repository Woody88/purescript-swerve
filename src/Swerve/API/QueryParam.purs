module Swerve.API.QueryParam where

import Prelude 

import Data.Int as Int
import Data.Maybe (Maybe(..))

data QueryParam (s :: Symbol) (a :: Type)

class ToQueryParam a where 
  toQueryParam :: a -> String 

instance toQueryParamString :: ToQueryParam String where 
  toQueryParam a = a 

instance toQueryParamInt :: ToQueryParam Int where 
  toQueryParam = show

class ReadQuery a where 
  readQuery :: String -> Maybe a 

instance readQueryString :: ReadQuery String where 
  readQuery x = Just x 

instance readQueryInt :: ReadQuery Int where 
  readQuery = Int.fromString

instance readQueryMaybe :: ReadQuery a => ReadQuery (Maybe a) where 
  readQuery = readQuery