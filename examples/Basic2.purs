module Examples.Basic2 where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp (defaultSettings, runSettings)
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.MediaType (PlainText)
import Swerve.API.Query (Query)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get)
import Swerve.Server (swerve)
import Type.Proxy (Proxy(..))

type SomeAPI = GetSomeEndpoint

type GetSomeEndpoint
  =  Get "/endpoint/:id/:action?[minAge]&[maxAge]"
  :> Capture "id" Int
  :> Capture "action" String
  :> Query "minAge" Int
  :> Query "maxAge" Int
  :> Resource String PlainText 

getSomeEndpoint :: {capture :: { id :: Int, action :: String }, query :: { minAge :: Int, maxAge :: Int }} ->  Aff String 
getSomeEndpoint conn = do 
  Console.logShow conn
  pure "HelloWorld" 
  
api =  getSomeEndpoint 

app :: Application
app = swerve (Proxy :: _ SomeAPI) api

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app