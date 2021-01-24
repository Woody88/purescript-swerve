module Test.Main where

import Prelude

import Data.Debug.Eval as D
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.HTTP.Types (hAuthorization, hContentType, ok200)
import Network.Wai (Application, Response(..), defaultRequest, responseStr) as Wai
import Network.Wai.Internal
import Swerve.API
import Swerve.Server 
import Swerve.Server (lift) as Server
import Test.Stream (newStream)
import Type.Proxy (Proxy(..))

type User = String
type UserId = Int  
type MaxAge = Int 
type Authorization = String 

type API = GetUser :<|> GetRaw 

type GetUser 
  = "users" 
  :> Capture "userId" UserId 
  :> QueryParam "maxAge" MaxAge 
  :> Header "authorization" Authorization 
  :> ReqBody PlainText String  
  :> Get JSON (Ok User + BadRequest + NotFound + Nil)

type GetRaw = "raw" :> Raw 

getUser 
  :: UserId 
  -> Maybe MaxAge 
  -> Authorization 
  -> String 
  -> Handler (Ok User + BadRequest + NotFound + Nil)
getUser userId _ _ body = case userId of 
  13        -> pure <<< respond (Proxy :: _ BadRequest') $ "Invalid User Id"
  17        -> pure <<< respond (Proxy :: _ NotFound') $ mempty
  otherwise -> do
    Console.log $ "Body: " <> body
    pure <<< respond (Proxy :: _ Ok') $ "User1"

getRaw :: HandlerM Wai.Application
getRaw = pure $ \req send -> send $ Wai.responseStr ok200 [] "Raw!"

server :: Server API
server = Server.lift (getUser :<|> getRaw)

app :: Wai.Application
app = serve (Proxy :: _ API) server

main :: Effect Unit
main = Aff.launchAff_ do 
  stream <- liftEffect $ newStream "Hello, World!"
  app (request stream) responseFn
  where 
    request s = wrap $ _ { body = Just s,  pathInfo = [ "users", "13" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple hContentType "text/plain", Tuple hAuthorization "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
    responseFn (Wai.ResponseString status headers message) = do 
      liftEffect $ D.eval { status, headers, message }
      pure ResponseReceived
    responseFn _ = liftEffect $ D.eval "bad response" *> pure ResponseReceived


-- -- -- app' :: Wai.Application
-- -- -- app' = toApplication $ runRouter (const err404) router

-- -- -- router' :: Router Unit 
-- -- -- router' = tweakResponse (map twk) router

-- -- -- twk :: Wai.Response -> Wai.Response
-- -- -- twk (Wai.ResponseString s hs b) = Wai.ResponseString (s { code = (s.code + 1) }) hs b
-- -- -- twk b = b

-- -- -- router :: Router Unit 
-- -- -- router = pathRouter "users" $ pathRouter "view" $ leafRouter $ \_

