module Test.Either where 

import Prelude 

import Data.Maybe 
import Data.Either
import Data.Either.Inject 
import Data.Either.Nested (type (\/))


x :: Int -> (String \/ Int \/ Boolean)
x n = case n of 
  2 -> inj (2 * 2)
  6 -> inj "hello"
  _ -> inj true
  

-- y :: Maybe Int 
-- y :: forall a b. All Eq b => Inject a b => b -> Maybe a
-- y = prj 