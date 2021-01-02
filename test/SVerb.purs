module Test.SVerb where

import Prelude hiding (Void)

import Effect 
import Effect.Class (liftEffect)
import Effect.Aff 
import Effect.Aff as Aff
import Data.Debug.Eval as D
import Data.Symbol
import Data.Either.Inject (inj)
import Data.Either.Nested (type  (\/))
import Data.Maybe 
import Data.Variant 
import Data.Variant as V
import Data.Newtype (wrap, unwrap)
import Data.Tuple 
import Network.HTTP.Types 
import Network.Wai as Wai
import Swerve.API 
import Swerve.Server
import Swerve.Server (lift) as Server
import Test.Stream (newStream)
import Type.Proxy
import Type.Row (type (+))

type LoginAPI
  = "login"
  :> Capture "id" Int 
  :> Get JSON (Ok String + BadRequest + Void)

login :: Int -> Handler (Ok String + BadRequest + Void)
login = pure <<< case _ of 
  13 -> respond (Proxy :: _ BadRequest') mempty
  _  -> respond (Proxy :: _ Ok') "Hello, World!"

loginAPI :: Server LoginAPI 
loginAPI = Server.lift login 

app :: Wai.Application
app = serve (Proxy :: _ LoginAPI) loginAPI

main :: Effect Unit
main = Aff.launchAff_ do 
  stream <- liftEffect $ newStream "Hello, World!"
  app (request stream) responseFn
  where 
    request s = wrap $ _ { body = Just s,  pathInfo = [ "login", "13" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple hContentType "text/plain", Tuple hAuthorization "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
    responseFn (Wai.ResponseString status headers message) = do 
      liftEffect $ D.eval { status, headers, message }
    responseFn _ = liftEffect $ D.eval "bad response"

-- login = pure $ case 1 of 
--   1 -> inj $ WithStatus (Proxy :: _ Ok') "Hello, World!"
--   _ -> inj $ WithStatus BadRequest NoContent