module Examples.Basic where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (asks)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.Wai (Application, responseStr)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.MediaType (JSON, PlainText)
import Swerve.API.Raw (Raw)
import Swerve.API.Spec (Capture, Header, Header', Query, ReqBody, Resource)
import Swerve.API.Verb (Post, Get)
import Swerve.Server (swerve)
import Swerve.Server.Internal.Handler (Handler)
import Swerve.Server.Internal.Header (withHeader)
import Swerve.Server.Internal.ServerError (err501)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type UserAPI = GetUser :<|> PostUser :<|> RawEndpoint 
type UserHandler 
    =  Handler GetUser String 
  :<|> Handler PostUser (Header' { hello :: String } HelloWorld)
  :<|> Handler RawEndpoint Application

type HelloWorld = { hello :: String }

type RawEndpoint = Raw "/random"
type GetUser = Get "/user"
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
getUser = pure "User" -- throwError err501

postUser :: Handler PostUser (Header' { hello :: String } HelloWorld) 
postUser = do 
    body <- asks $ _.body
    Console.log $ "Recieved: " <> body
    withHeader { hello: "world!" } { hello: "World!" }

rawEndpoint :: Handler RawEndpoint Application 
rawEndpoint = pure $ \req resp -> do 
    resp $ responseStr ok200 [] "Hello from Raw endpoint!"

api :: UserHandler
api =  getUser :<|> postUser :<|> rawEndpoint

app :: Application
app = swerve (Proxy :: _ UserAPI) api

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app