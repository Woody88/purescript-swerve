module Examples.Basic where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.Wai (Application, Request(..), responseStr)
import Network.Warp (defaultSettings, runSettings)
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.Guard (Guard)
import Swerve.API.Header (Header, Headers(..), withHeaders)
import Swerve.API.MediaType (PlainText)
import Swerve.API.Query (Query)
import Swerve.API.Raw (Raw)
import Swerve.API.ReqBody (ReqBody)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get, Post)
import Swerve.Server (swerve, swerveContext)
import Type.Proxy (Proxy(..))

type SomeAPI = GetSomeEndpoint

type GetSomeEndpoint
 =  Get "/user"
 :> Header "shigeta-san" String 
 :> Resource String PlainText 
 
-- type GetSomeEndpoint
--   = Post "/endpoint/:id?[maxAge]"
--   :> Capture "id" Int 
--   :> Query "maxAge" Int
--   :> Header "token" String
--   :> ReqBody String PlainText 
--   :> Resource String PlainText 

-- getSomeEndpoint :: {capture :: { id :: Int }, query :: {maxAge :: Int}, header :: {token :: String}, body :: String} -> Aff String
-- getSomeEndpoint conn = pure $ 
--     "Hello, Endpoint " <> 
--     show conn.capture.id <> 
--     " with maxAge: " <> 
--     show conn.query.maxAge <> 
--     " with token: " <>
--     conn.header.token <>
--     "with body: " <> 
--     conn.body <>
--     "!"

getSomeEndpoint :: {header :: { "shigeta-san" :: String }} -> Aff String 
getSomeEndpoint conn = do
  Console.logShow conn 
  pure "Hello, World!"

api = getSomeEndpoint 

app :: Application
app = swerve (Proxy :: _ SomeAPI) api

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop } app