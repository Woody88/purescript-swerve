module Examples.Main where

import Prelude

import Control.Monad.Reader (class MonadReader, ReaderT(..), ask, asks, lift, runReaderT)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.MediaType (JSON, PlainText)
import Swerve.API.Spec (Capture, Header, Header', Query, ReqBody, Resource)
import Swerve.API.Verb (Post, Get)
import Swerve.Server (swerve)
import Swerve.Server.Internal.Handler (Handler, HandlerT(..))
import Swerve.Server.Internal.Header (withHeader)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type App = ReaderT String Aff

type UserAPI = GetUser :<|> PostUser
type UserHandler = HandlerT GetUser App String :<|> HandlerT PostUser App (Header' { hello :: String } HelloWorld)

type HelloWorld = { hello :: String }

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
 
getUser :: HandlerT GetUser App String 
getUser = pure "User"

postUser :: HandlerT PostUser App (Header' { hello :: String } HelloWorld) 
postUser = do 
    body <- asks $ _.body
    a <- lift $ ask
    Console.log $ "Recieved: " <> body
    Console.log $ "Env: " <> a 
    f <- lift $ myfunc
    Console.log $ "func val: " <> f
    withHeader { hello: "world!" } { hello: "World!" }

api :: UserHandler
api =  getUser :<|> postUser

app :: Application
app = swerve (Proxy :: _ UserAPI) (flip runReaderT "hello buy") api

myfunc :: App String 
myfunc = do 
    env <- ask
    Console.log $ "Inner env: " <> env 
    pure "goodbye"

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app