module Test.Main where

import Prelude

import Data.Debug.Eval as D
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Network.Wai (Application, Response(..), defaultRequest) as Wai
import Network.Wai (responseStr)
import Swerve.API.ContentType (JSON)
import Swerve.API.Types (type (:>), Capture, Header, QueryParam)
import Swerve.API.Verb (Get)
import Swerve.Server.Internal (Server, serve)
import Swerve.Server.Internal (from) as Server
import Swerve.Server.Internal.Response (Response)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.Router (Router, leafRouter, pathRouter, runRouter, tweakResponse)
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Swerve.Server.Internal.ServerError (err404)
import Type.Proxy (Proxy(..))

type User = String
type UserId = Int  
type MaxAge = Int 
type Authorization = String 

type GetUser = "users" :> Capture UserId :> QueryParam "maxAge" MaxAge :> Header "authorization" Authorization :> Get User JSON

getUser :: UserId -> Maybe MaxAge -> Authorization -> Aff (Response _ User) 
getUser _ _ _ = pure $ pure "Woody"

server :: Server GetUser 
server = Server.from getUser

app :: Wai.Application 
app = serve (Proxy :: _ GetUser) server

-- app' :: Wai.Application
-- app' = toApplication $ runRouter (const err404) router

-- router' :: Router Unit 
-- router' = tweakResponse (map twk) router

-- twk :: Wai.Response -> Wai.Response
-- twk (Wai.ResponseString s hs b) = Wai.ResponseString (s { code = (s.code + 1) }) hs b
-- twk b = b

-- router :: Router Unit 
-- router = pathRouter "users" $ pathRouter "view" $ leafRouter $ \_ _ cont -> cont (Route $ responseStr ({code: 201, message: ""}) [] "")

main :: Effect Unit
main = Aff.launchAff_ do 
  app request responseFn
  where 
    request = wrap $ _ { pathInfo = [ "users", "13" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple (wrap "authorization") "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
    responseFn (Wai.ResponseString status headers message) = liftEffect $ D.eval { status, headers, message }
    responseFn _ = liftEffect $ D.eval "bad response"