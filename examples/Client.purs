module Examples.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.Header (Header)
import Swerve.API.MediaType (PlainText)
import Swerve.API.Query (Query)
import Swerve.API.ReqBody (ReqBody)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get, Post)
import Swerve.Client.ClientM (ClientM(..), runClientM)
import Swerve.Client.Internal (client)
import Swerve.Client.Internal.HasClient (class HasClient)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type GetSomeEndpoint
  = Post "/endpoint/:id?[maxAge]"
  :> Capture "id" Int
  :> Query "maxAge" Int
  :> Header "token" String
  :> ReqBody String PlainText
  :> Resource String PlainText

getUser :: ClientM String 
getUser = client (Proxy :: _ GetSomeEndpoint) {capture: {id: 13}, query: {maxAge: 30}, header: {token: "x123dsa4!%dsa&"}, body: "Hello, World"}

main :: Effect Unit 
main = launchAff_ do 
  result <- runClientM getUser "http://localhost:3000"
  Console.logShow result
