module Test.SVerb where

import Prelude 

import Effect.Aff
import Data.Either.Inject (inj)
import Data.Either.Nested (type  (\/))
import Network.Wai as Wai
import Swerve.API (type (:>), GET', JSON, SVerb, WithStatus(..), Ok', NoContent)
import Swerve.API.SVerb
import Swerve.Server
import Swerve.Server (lift) as Server
import Type.Proxy

type LoginAPI
  = "login"
  :> SVerb GET' JSON (Ok String \/ Void)


login :: Aff (Ok String \/ Void)
login = pure $ inj $ WithStatus (Proxy :: _ Ok') "Hello, World!"

loginAPI :: Server LoginAPI 
loginAPI = Server.lift login 

app :: Wai.Application
app = serve (Proxy :: _ LoginAPI) loginAPI

-- main :: Effect Unit
-- main = Aff.launchAff_ do 
--   stream <- liftEffect $ newStream "Hello, World!"
--   app (request stream) responseFn
--   where 
--     request s = wrap $ _ { body = Just s,  pathInfo = [ "users", "13" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple hContentType "text/plain", Tuple hAuthorization "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
--     responseFn (Wai.ResponseString status headers message) = do 
--       liftEffect $ D.eval { status, headers, message }
--     responseFn _ = liftEffect $ D.eval "bad response"

-- login = pure $ case 1 of 
--   1 -> inj $ WithStatus (Proxy :: _ Ok') "Hello, World!"
--   _ -> inj $ WithStatus BadRequest NoContent