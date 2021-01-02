module Test.Basic where

import Prelude 

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff as Aff
import Data.Debug.Eval as D
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..))
import Network.HTTP.Types (hAuthorization, hContentType)
import Network.Wai as Wai
import Swerve.API 
import Swerve.Server
import Swerve.Server (lift) as Server
import Test.Stream (newStream)
import Type.Proxy (Proxy(..))

type LoginAPI
  = "login"
  :> Capture "id" Int 
  :> Get JSON (Ok String + BadRequest + Nil)

login :: Int -> Handler (Ok String + BadRequest + Nil)
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