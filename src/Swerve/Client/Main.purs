module Swerve.Client.Main where 

import Prelude 

import Data.Either
import Data.Maybe 
import Effect (Effect)
import Effect.Class.Console as Console 
import Network.HTTP.Media
import Swerve.API.Alternative (type (:))
import Swerve.API.ContentType 
import Type.Proxy 

type CTs = (JSON : PlainText)

type B = {body :: String}

main :: Effect Unit 
main = do 
  let (x :: Either String String)  = case canHandleCTypeH (Proxy :: _ CTs) "application/json" of 
                                        Nothing -> Left "bad"
                                        Just f  -> f "\"yo\""
  Console.logShow x
    
main2 :: Array MediaType
main2 = allMime (Proxy :: _ CTs)