module Test.Main where

import Prelude

import Data.Debug.Eval as D
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Network.Wai (Application, Response(..), defaultRequest) as Wai
import Swerve.API.ContentType (JSON)
import Swerve.API.Types (type (:>))
import Swerve.API.Verb (Get)
import Swerve.Server.Internal (Server, serve)
import Swerve.Server.Internal (from) as Server
import Swerve.Server.Internal.Response (Response)
import Type.Proxy (Proxy(..))

type User = String 
type GetUser = "users" :> Get User JSON

getUser :: Aff (Response _ User) 
getUser = pure $ pure "Woody"

server :: Server GetUser 
server = Server.from getUser

app :: Wai.Application 
app = serve (Proxy :: _ GetUser) server

main :: Effect Unit
main = Aff.launchAff_ do 
  app request responseFn
  where 
    request = wrap $ _ { url = "/user", headers = [Tuple (wrap "authorization") "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
    responseFn (Wai.ResponseString status headers message) = liftEffect $ D.eval { status, headers, message }
    responseFn _ = liftEffect $ D.eval "bad response"