# Swerve 

Swerve is a type-level server and client library heavily inspired by Haskell's Servant library. 
A major, difference is the use of row types instead of introducing numerous combinators. 

## Installation

***This library is not yet published to pursuit.***  
***You will need to use purescript 0.14***  
You can install this package by adding the following in your packages.dhall:

<details>


```dhall
let additions =
    { warp =
        { dependencies =
          [ "node-fs-aff"
          , "node-net"
          , "node-url"
          , "wai"
          ]
        , repo =
            "https://github.com/Woody88/purescript-warp.git"
        , version =
            "master"
        }
    , debugged =
        { dependencies =
            [ "console"
            , "effect"
            , "prelude"
            , "strings"
            , "record"
            , "ordered-collections"
            , "either"
            , "tuples"
            , "lists"
            , "arrays"
            , "bifunctors"
            , "generics-rep"
            , "datetime"
            , "enums"
            ]
        , repo =
            "https://github.com/Woody88/purescript-debugged.git"
        , version =
            "ps-0.14"
        } 
    , wai =
        { dependencies =
            [ "http-types"
            , "node-buffer"
            , "node-http"
            , "node-net"
            , "node-streams"
            , "node-url"
            ]
        , repo =
            "https://github.com/Woody88/purescript-wai.git"
        , version =
            "master"
        }
    , http-types =
        { dependencies =
            [ "console"
            , "effect"
            , "psci-support"
            , "tuples"
            , "unicode"
            , "uri"
            ]
        , repo =
            "https://github.com/Woody88/purescript-http-types.git"
        , version =
            "master"
        }
    , http-media =
        { dependencies =
            [ "console"
            , "effect"
            , "exceptions"
            , "foldable-traversable"
            , "maybe"
            , "newtype"
            , "numbers"
            , "ordered-collections"
            , "proxy"
            , "strings"
            , "stringutils"
            , "unicode"
            ]
        , repo =
            "https://github.com/Woody88/purescript-http-media.git"
        , version =
            "master"
        }
```
```console
user@user:~$ spago install swerve
```
</details>

## Usage 

### Basic Example
For more examples please refer to the test folder.

```purescript 
import Prelude

import Data.Debug.Eval as D
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.HTTP.Types (hAuthorization, hContentType, ok200)
import Network.Wai (Application, Response(..), defaultRequest, responseStr) as Wai
import Swerve.API
import Swerve.Server 
import Swerve.Server (lift) as Server
import Test.Stream (newStream)
import Type.Proxy (Proxy(..))

type User = String
type UserId = Int  
type MaxAge = Int 
type Authorization = String 

type API = GetUser :<|> GetRaw 

type GetUser 
  = "users" 
  :> Capture "userId" UserId 
  :> QueryParam "maxAge" MaxAge 
  :> Header "authorization" Authorization 
  :> ReqBody PlainText String  
  :> Get JSON (Ok User + BadRequest + NotFound + Nil)

type GetRaw = "raw" :> Raw 

getUser 
  :: UserId 
  -> Maybe MaxAge 
  -> Authorization 
  -> String 
  -> Handler (Ok User + BadRequest + NotFound + Nil)
getUser userId _ _ body = case userId of 
  13        -> pure <<< respond (Proxy :: _ BadRequest') $ "Invalid User Id"
  17        -> pure <<< respond (Proxy :: _ NotFound') $ mempty
  otherwise -> do
    Console.log $ "Body: " <> body
    pure <<< respond (Proxy :: _ Ok') $ "User" <> show userId

getRaw :: HandlerM Wai.Application
getRaw = pure $ \req send -> send $ Wai.responseStr ok200 [] "Raw!"

server :: Server API
server = Server.lift (getUser :<|> getRaw)

app :: Wai.Application
app = serve (Proxy :: _ API) server

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app
```         