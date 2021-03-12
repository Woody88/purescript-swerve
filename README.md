[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/Woody88/purescript-swerve/blob/master/LICENSE)
![CI](https://github.com/Woody88/purescript-swerve/workflows/CI/badge.svg?branch=master)
# Swerve 

Swerve is a type-level server and client library heavily inspired by Haskell's Servant library. 
A major, difference is the use of row types instead of introducing numerous combinators. 

## Installation

***This library is not yet published to pursuit.***  
***You will need to use purescript 0.14 and above***  
You can install this package by adding the following in your packages.dhall:

<details>


```dhall
in  upstream
  with b64 = 
    { dependencies =
      [ "console", "effect", "node-fs-aff", "wai", "encoding" ]
    , repo = "https://github.com/CarstenKoenig/purescript-b64.git"
    , version = "purescript-0.14"
    }
  with warp =
    { dependencies =
      [ "console", "effect", "node-fs-aff", "wai" ]
    , repo = "https://github.com/Woody88/purescript-warp.git"
    , version = "master"
    }
  with debugged =
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
      , "datetime"
      , "enums"
      ]
    , repo = "https://github.com/Woody88/purescript-debugged.git"
    , version = "ps-0.14"
    }
  with wai =
    { dependencies = [ "effect", "aff", "http-types", "node-net" ]
    , repo = "https://github.com/Woody88/purescript-wai.git"
    , version = "master"
    }
  with http-types =
    { dependencies = [ "js-uri", "tuples", "unicode" ]
    , repo = "https://github.com/Woody88/purescript-http-types.git"
    , version = "master"
    }
  with http-media =
    { dependencies =
      [ "console"
      , "effect"
      , "exceptions"
      , "foldable-traversable"
      , "maybe"
      , "newtype"
      , "numbers"
      , "ordered-collections"
      , "psci-support"
      , "strings"
      , "stringutils"
      , "unicode"
      ]
    , repo = "https://github.com/Woody88/purescript-http-media.git"
    , version = "master"
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
import Network.Warp.Settings (defaultSettings)
import Network.Warp.Run (runSettings)
import Swerve.API
import Swerve.Server 
import Swerve.Server (lift, compose) as Server
import Test.Stream (newStream)
import Type.Proxy (Proxy(..))

-- API Spec
type API = Record 
    ( users :: "users" :> UserAPI 
    , raw   :: RawApp
    )

type UserAPI = Record 
    ( get :: GetUser 
    )

type RawApp = "raw" :> Raw 

type User = String
type UserId = Int  
type MaxAge = Int 
type Authorization = String 

type GetUser 
  =  Capture "userId" UserId 
  :> QueryParam "maxAge" MaxAge 
  :> Header "authorization" Authorization 
  :> ReqBody PlainText String  
  :> Get JSON (Ok User + BadRequest + NotFound + Nil)

-- Handlers 
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

rawApp :: HandlerM Wai.Application
rawApp = pure $ \req send -> send $ Wai.responseStr ok200 [] "Hello, World!"

-- Servers 
raw :: Server RawApp 
raw = Server.lift rawApp 

users :: Server UserAPI
users = Server.lift { get: getUser } 

server :: Server API 
server = Server.compose { users, raw }

-- WAI Application
app :: Wai.Application
app = serve (Proxy :: _ API) server

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app
```         
