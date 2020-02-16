module Examples.BasicServer where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Swerve.API.MediaType (JSON)
import Swerve.API.ContentTypes (JSON) as CT
import Swerve.Server (swerve)
import Swerve.Server.Internal (Server(..))
import Swerve.Server.Internal.Handler (Handler(..))
import Swerve.Server.Internal.Route (Get)

type User = {name :: String, id :: Int }

type GetUser = Get "/user" User JSON ("content-type" :: CT.JSON, "accept" :: CT.JSON)

getUser :: Handler GetUser User 
getUser = pure $  {name: "Woodson", id: 5 }

type UserAPI = (getUser :: GetUser)

api :: Server UserAPI
api = Server

app :: Application 
app = swerve api { getUser: \_ -> getUser }

main :: Effect Unit
main = do 
    let beforeMainLoop = do 
            Console.log $ "Listening on port " <> show defaultSettings.port
    runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app


