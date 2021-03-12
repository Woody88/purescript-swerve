module Test.API.Composition where

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
import Network.Wai.Internal
import Swerve.API
import Swerve.Server
import Swerve.Server (compose, lift) as Server
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type API = Record 
  ( loginAPI :: LoginAPI 
  , userAPI  :: UserAPI 
  )

type LoginAPI = Record 
  ( login  :: Login
  , logout :: Logout 
  )

type Login 
  = "login" 
  :> Get JSON (Ok String + Nil) 

type Logout 
  = "logout" 
  :> Get JSON (Ok String + Nil) 

type UserAPI = Record
  ( user  :: User
  , users :: Users
  ) 

type User 
  = "user" 
  :> Get JSON (Ok String + Nil)

type Users 
  = "users" 
  :> Get JSON (Ok String + Nil)

login :: Handler (Ok String + Nil) 
login = pure <<< respond (Proxy :: _ Ok') $  "login"

logout :: Handler (Ok String + Nil) 
logout = pure <<< respond (Proxy :: _ Ok') $  "logout"

user :: Handler (Ok String + Nil)
user = pure <<< respond (Proxy :: _ Ok') $ "user"

users :: Handler (Ok String + Nil)
users = pure <<< respond (Proxy :: _ Ok') $ "users"

loginAPI :: Server LoginAPI
loginAPI = Server.lift { login, logout }

userAPI :: Server UserAPI 
userAPI = Server.lift { user, users }

server :: Server API
server = Server.compose { loginAPI,  userAPI }

app :: Wai.Application
app = serve (Proxy :: _ API) server