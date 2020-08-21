module Main where

import Prelude

import Control.Monad.Reader (asks)
import Effect (Effect)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Swerve.API.ContentTypes (NoContent(..))
import Swerve.API.Spec (Capture)
import Swerve.API.Verb (GetNoContent)
import Swerve.Server (swerve)
import Swerve.Server.Internal.Handler (Handler)
import Type.Proxy (Proxy(..))

type UserAPI = GetUser

type GetUser 
    = GetNoContent "/user/:id" 
        ( Capture { id :: Int }
        )

getUser :: Handler GetUser NoContent  
getUser = do 
    num <- asks _.capture.id
    pure NoContent

server = getUser

api :: Application
api = swerve (Proxy :: _ UserAPI) server

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } api