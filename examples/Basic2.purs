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
import Swerve.API.Combinators (type (:>))
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

type SomeAPI = Guard "apiKey" ApiKey :> GetSomeEndpoint

type GetSomeEndpoint
  = Get "/endpoint/:id"
  :> Capture "id" Int 
  :> Resource (Headers (token :: String) String) PlainText 

getSomeEndpoint :: ApiKey -> {capture :: {id :: Int}} -> Aff (Headers (token :: String) String)
getSomeEndpoint apikey conn = do 
  Console.log $ "key: " <> apikey
  pure $ withHeaders {token: apikey} "Hello, world!"

api = getSomeEndpoint 

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