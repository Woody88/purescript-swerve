module Test.Either where 

import Prelude 

import Effect 
import Effect.Class.Console as Console
import Data.Maybe 
import Data.Either
import Data.Either.Inject 
import Data.Either.Nested (type (\/))
import Unsafe.Coerce

x :: Int -> (String \/ Int \/ Boolean)
x n = case n of 
  2 -> inj (2 * 2)
  6 -> inj "hello"
  _ -> inj true
  

y :: forall a. Inject a (String \/ Int \/ Boolean) => Maybe a
y = prj (x 6)

data Resp  

-- unDelayed ::
--   forall env c r.
--   ( forall captures auth contentType params headers body.
--     DelayedSpec env captures auth contentType params headers body c -> 
--     r
--   ) ->
--   Delayed env c ->
--   r

xs :: forall r. r -> r 
xs = unsafeCoerce 

z :: Maybe String
z = xs y


main :: Effect Unit 
main = Console.logShow z


