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
import Swerve.API.ContentTypes (JSON, NoContent(..), PlainText)
import Swerve.API.Spec (Capture, ContentType, Header, Header'(..), Query, ReqBody, ReqBody'(..), Resource, withHeader)
import Swerve.API.Verb (GetNoContent, PostNoContent, Post)
import Swerve.Server (swerve)
import Swerve.Server.Internal.Handler (Handler)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type UserAPI = GetUser

type HelloWorld = { hello :: String }

type GetUser 
    = Post "/user/:id?[maxAge]&[minAge]" 
        ( Capture { id :: Int }
        + Query { maxAge :: Maybe Int, minAge :: Maybe Int } 
        + Header { accept :: String }
        + ReqBody String PlainText
        + Resource (Header' { hello :: String } HelloWorld) JSON
        + ()
        )

postUser :: Handler GetUser (Header' { hello :: String } HelloWorld) 
postUser = do 
    accept <- asks $ _.header.accept
    Console.log accept
    withHeader { hello: "world!" } { hello: "World!" }

server :: _
server = postUser

api :: Application
api = swerve (Proxy :: _ UserAPI) server

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } api