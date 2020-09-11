module Examples.CustomMonad where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, ask, asks, lift, runReaderT)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Wai (Application)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.MediaType (JSON, PlainText)
import Swerve.API.Spec (Capture, Header, Header', Query, ReqBody, Resource)
import Swerve.API.Verb (Post, Get)
import Swerve.Server (swerveHoist)
import Swerve.Server.Internal.Handler (HandlerT)
import Swerve.Server.Internal.Header (withHeader)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type App = (ReaderT String (ExceptT String Aff))
type Handler specs a = HandlerT specs App a

type UserAPI = GetUser :<|> PostUser
type UserHandler = Handler GetUser String :<|> Handler PostUser (Header' { hello :: String } HelloWorld)

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
 
getUser :: Handler GetUser String 
getUser = pure "User"

postUser :: Handler PostUser (Header' { hello :: String } HelloWorld) 
postUser = do 
    body <- asks $ _.body
    a <- lift $ ask
    Console.log $ "Recieved: " <> body
    Console.log $ "Env: " <> a 
    f <- lift $ myfunc
    Console.log $ "myfunc val: " <> f
    withHeader { hello: "world!" } { hello: "World!" }

api :: UserHandler
api =  getUser :<|> postUser

app :: Application
app = swerveHoist (Proxy :: _ UserAPI) (runExceptT <<< flip runReaderT "hello") api

myfunc :: App String 
myfunc = do 
    env <- ask
    Console.log $ "Inner env: " <> env 
    pure "goodbye"

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app