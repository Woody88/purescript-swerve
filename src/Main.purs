module Main where

import Prelude

import Control.Monad.Reader (asks)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Node.FS (FileFlags(..))
import Swerve.API.ContentTypes (NoContent(..), PlainText)
import Swerve.API.Spec (Capture, Header, Query, ReqBody, ReqBody'(..), ContentType)
import Swerve.API.Verb (GetNoContent, PostNoContent)
import Swerve.Server (swerve)
import Swerve.Server.Internal.Handler (Handler)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type UserAPI = GetUser

type GetUser 
    = PostNoContent "/user/:id?[maxAge]&[minAge]" 
        ( Capture { id :: Int }
        + Query { maxAge :: Maybe Int, minAge :: Maybe Int } 
        -- + ReqBody String PlainText
        + ()
        )

getUser :: Handler GetUser NoContent  
getUser = do  
    pure NoContent

server = getUser

api :: Application
api = swerve (Proxy :: _ UserAPI) server

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } api