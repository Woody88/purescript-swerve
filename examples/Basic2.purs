module Examples.Basic2 where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.Wai (Application, Request(..), responseStr)
import Network.Warp (defaultSettings, runSettings)
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>), type (:<|>), (:<|>))
import Swerve.API.Guard (Guard)
import Swerve.API.Header (Header, Headers(..), withHeaders)
import Swerve.API.MediaType (PlainText)
import Swerve.API.Query (Query)
import Swerve.API.Raw (Raw)
import Swerve.API.ReqBody (ReqBody)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get, Post)
import Swerve.Server (swerve, swerveContext)
import Type.Proxy (Proxy(..))

type ApiKey = String 

type SomeAPI = Guard "apiKey" ApiKey :> (UserAPI :<|> Raw "/raw")

type UserAPI = GetSomeEndpoint 
type UserHandlers = GetSomeEndpointHandler :<|> Application 

type GetSomeEndpoint
 = Post "/user/:id?[maxAge]"
 :> Capture "id" Int 
 :> Query "maxAge" Int 
 :> ReqBody String PlainText
 :> Resource (Headers (token :: String) String) PlainText 

type GetSomeEndpointHandler = {capture :: {id :: Int}, query :: {maxAge :: Int }, body :: String } -> Aff (Headers (token :: String) String) 

getSomeEndpoint :: {capture :: {id :: Int}, query :: {maxAge :: Int }, body :: String } -> Aff (Headers (token :: String) String)
getSomeEndpoint conn = do 
  -- Console.log $ "key: " <> apikey
  pure $ withHeaders {token: "apikey"} "Hello, world!"

endpointRaw :: Application
endpointRaw req send = send $ responseStr ok200 [] "Rawww!"

api :: ApiKey -> UserHandlers 
api apikey = getSomeEndpoint :<|> endpointRaw

apiKey :: Request -> Aff (Either String ApiKey)
apiKey (Request req) = pure $ case apiKey' of 
  Nothing   -> Left "Need apikey!"
  Just key -> Right key  
  where 
    apiKey' = Map.lookup (wrap "apiKey") $ Map.fromFoldable $ req.headers

app :: Application
app = swerveContext (Proxy :: _ SomeAPI) { apiKey } api

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app