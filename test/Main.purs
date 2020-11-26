module Test.Main where

import Prelude

import Data.Debug.Eval as D
import Data.Either (Either)
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
import Node.Stream (Readable)
import Swerve.API.ContentType (JSON, PlainText)
import Swerve.API.Status (BadRequest, NotFound, Ok, _BadRequest, _NotFound, _Ok)
import Swerve.API.Types (type (:<|>), type (:>), Capture, Header, QueryParam, Raise, Raw, ReqBody, Respond, Respond', Spec, (:<|>))
import Swerve.API.Verb (Get)
import Swerve.Server.Internal (class HasServer, Server, route, serve, toHandler)
import Swerve.Server.Internal (from) as Server
import Swerve.Server.Internal.Response (class HasResp, Response(..), raise, respond)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.Router (Router, leafRouter, pathRouter, runRouter, tweakResponse)
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Swerve.Server.Internal.ServerError (err404)
import Type.Proxy (Proxy(..))

type User = String
type UserId = Int  
type MaxAge = Int 
type Authorization = String 

type API = GetUser :<|> GetRaw 

type GetUser 
  = "users" 
  :> Capture UserId 
  :> QueryParam "maxAge" MaxAge 
  :> Header "authorization" Authorization 
  :> ReqBody String PlainText
  :> Raise BadRequest () JSON
  :> Raise NotFound () JSON
  :> Get User Ok () JSON 

type GetRaw = "raw" :> Raw 

getUser :: forall rs
  .  HasResp Ok () rs
  => HasResp BadRequest () rs
  => HasResp NotFound () rs
  => UserId 
  -> Maybe MaxAge 
  -> Authorization 
  -> String -> Aff (Response rs User) 
getUser userId _ _ body = case userId of 
  13        -> pure $ raise _BadRequest
  17        -> pure $ raise _NotFound
  otherwise -> do
    Console.log $ "Body: " <> body
    pure $ respond _Ok "User1"

getRaw :: Wai.Application
getRaw req send = send $ Wai.responseStr ok200 [] "Raw!"

server :: Server API
server = Server.from (getUser :<|> getRaw)

app :: Wai.Application 
app = serve (Proxy :: _ API) server

main :: Effect Unit
main = Aff.launchAff_ do 
  stream <- liftEffect $ newStream "Hello, World!"
  app (request stream) responseFn
  where 
    request s = wrap $ _ { body = Just s,  pathInfo = [ "users", "17" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple hContentType "text/plain", Tuple hAuthorization "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
    responseFn (Wai.ResponseString status headers message) = liftEffect $ D.eval { status, headers, message }
    responseFn _ = liftEffect $ D.eval "bad response"

foreign import newStream :: String -> Effect (Readable ())
-- -- app' :: Wai.Application
-- -- app' = toApplication $ runRouter (const err404) router

-- -- router' :: Router Unit 
-- -- router' = tweakResponse (map twk) router

-- -- twk :: Wai.Response -> Wai.Response
-- -- twk (Wai.ResponseString s hs b) = Wai.ResponseString (s { code = (s.code + 1) }) hs b
-- -- twk b = b

-- -- router :: Router Unit 
-- -- router = pathRouter "users" $ pathRouter "view" $ leafRouter $ \_ _ cont -> cont (Route $ responseStr ({code: 201, message: ""}) [] "")