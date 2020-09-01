module Examples.Main where

import Prelude

import Control.Monad.Reader (asks)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.MediaType (JSON, PlainText)
import Swerve.API.Spec (Capture, Header, Header', Query, ReqBody, Resource)
import Swerve.API.Verb (Post, Get)
import Swerve.Server (swerve)
import Swerve.Server.Internal.Handler (Handler)
import Swerve.Server.Internal.Header (withHeader)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type UserAPI = GetUser :<|> PostUser

type HelloWorld = { hello :: String }

type GetUser = Get "/users"
    ( Resource String PlainText
    + ()
    )

type PostUser = Post "/user/:id?[maxAge]&[minAge]" 
    ( Capture { id :: Int }
    + Query { maxAge :: Maybe Int, minAge :: Maybe Int } 
    + Header { accept :: String }
    + ReqBody String PlainText
    + Resource (Header' { hello :: String } HelloWorld) JSON
    + ()
    )
 
getUser :: Handler GetUser String 
getUser = pure "Woodson"

postUser :: Handler PostUser (Header' { hello :: String } HelloWorld) 
postUser = do 
    accept <- asks $ _.header.accept
    Console.log accept
    withHeader { hello: "world!" } { hello: "World!" }

server :: _
server =  getUser :<|> postUser

api :: Application
api = swerve (Proxy :: _ UserAPI) server

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } api