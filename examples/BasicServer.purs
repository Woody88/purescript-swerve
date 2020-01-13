module Examples.BasicServer where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Swerve.API.MediaType (JSON)
import Swerve.Server (swerve)
import Swerve.Server.Internal (Server(..))
import Swerve.Server.Internal.Handler (Handler(..))
import Swerve.Server.Internal.Route (Get)

type User = {name :: String, id :: Int }

type GetUser = Get "/user/{id:Int}" User JSON ()

getUser :: {params :: { id :: Int} } -> Handler GetUser User 
getUser conn = pure $  {name: "Woodson", id: conn.params.id }

type UserAPI = (getUser :: GetUser)

api :: Server UserAPI
api = Server

app :: Application 
app = swerve api { getUser: getUser }

main :: Effect Unit
main = do 
    let beforeMainLoop = do 
            Console.log $ "Listening on port " <> show defaultSettings.port
    runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app


