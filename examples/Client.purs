module Examples.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.MediaType (PlainText)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (Get)
import Swerve.Client.ClientM (ClientM(..), runClientM)
import Swerve.Client.Internal (client)
import Swerve.Client.Internal.HasClient (class HasClient)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type GetSomeEndpoint
  = Get "/endpoint/:id"
  :> Capture "id" Int
  :> Resource String PlainText

getUser :: ClientM String 
getUser = client (Proxy :: _ GetSomeEndpoint) {capture: {id: 13 }}

main :: Effect Unit 
main = launchAff_ do 
  result <- runClientM getUser "http://localhost:3000"
  Console.logShow result
