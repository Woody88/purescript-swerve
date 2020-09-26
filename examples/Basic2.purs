module Examples.Basic2 where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.Wai (Application, responseStr)
import Network.Warp (defaultSettings, runSettings)
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.Header (Header)
import Swerve.API.MediaType (PlainText)
import Swerve.API.Query (Query)
import Swerve.API.Raw (Raw)
import Swerve.API.ReqBody (ReqBody)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get, Post)
import Swerve.Server (swerve)
import Type.Proxy (Proxy(..))

type SomeAPI = GetSomeEndpoint

type GetSomeEndpoint
  =  Raw "/endpoint"

getSomeEndpoint :: Application
getSomeEndpoint req send = do 
  send $ responseStr ok200 [] "Hello, world!"

api =  getSomeEndpoint 

app :: Application
app = swerve (Proxy :: _ SomeAPI) api

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app