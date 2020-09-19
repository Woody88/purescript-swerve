module Examples.Basic2 where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp (defaultSettings, runSettings)
import Swerve.API.Combinators (type (:>))
import Swerve.API.MediaType (PlainText)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get)
import Swerve.Server (swerve)
import Type.Proxy (Proxy(..))

type UserAPI = GetUser

type GetUser 
  =  Get "/user"
  :> Resource String PlainText 

getUser :: Aff String 
getUser = pure "Woodson" 
  
api =  getUser 

app :: Application
app = swerve (Proxy :: _ UserAPI) api

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app