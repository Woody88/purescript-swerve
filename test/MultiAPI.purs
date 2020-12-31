module Test.MultiAPI where

import Prelude

import Data.Debug.Eval as D
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.HTTP.Types (hAuthorization, hContentType, ok200)
import Network.Wai (Application, Response(..), defaultRequest, responseStr) as Wai
import Swerve.API
import Swerve.API (type (:<|>), type (:>), BadRequest, Capture, Get, Header, JSON, NotFound, Ok, PlainText, QueryParam, Raise, Raw, ReqBody, _BadRequest, _NotFound, _Ok, (:<|>))
import Swerve.Server (Response, raise, respond, serve)
import Swerve.Server (Server, ServerT)
import Swerve.Server (eval, compose, lift) as Server
import Test.Stream (newStream)
import Type.Proxy (Proxy(..))

type API = LoginAPI :<|> UserAPI 

type LoginAPI = Login :<|> Logout 

type Login 
  = "login" 
  :> Get String JSON 

type Logout 
  = "logout" 
  :> Get String JSON 

type UserAPI = User :<|> Users 

type User 
  = "user" 
  :> Get String JSON 

type Users 
  = "users" 
  :> Get String JSON 

login :: Aff (Response _ String) 
login = pure $ respond _Ok "login"

logout :: Aff (Response _ String) 
logout = pure $ raise _BadRequest

user :: Aff (Response _ String) 
user = pure $ respond _Ok "user"

users :: Aff (Response _ String) 
users = pure $ respond _Ok "users"

loginAPI :: Server LoginAPI
loginAPI = Server.lift (login :<|> logout)

userAPI :: Server UserAPI 
userAPI = Server.lift (user :<|> users)

server :: Server API
server = Server.eval (loginAPI :<|> userAPI)

app :: Wai.Application
app = serve (Proxy :: _ API) server

main :: Effect Unit
main = Aff.launchAff_ do 
  stream <- liftEffect $ newStream "Hello, World!"
  app (request stream) responseFn
  where 
    request s = wrap $ _ { body = Just s,  pathInfo = [ "login" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple hContentType "text/plain", Tuple hAuthorization "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
    responseFn (Wai.ResponseString status headers message) = do 
      liftEffect $ D.eval { status, headers, message }
    responseFn _ = liftEffect $ D.eval "bad response"