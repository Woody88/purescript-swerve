module Examples.Client where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Swerve.API.MediaType (PlainText)
import Swerve.API.Spec (Resource)
import Swerve.API.Verb (Get)
import Swerve.Client.ClientM (ClientM(..), runClientM)
import Swerve.Client.Internal (client)
import Swerve.Client.Internal.HasClient (class HasClient)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type GetUser = Get "/user"
    ( Resource String PlainText
    + ()
    )

getUser :: ClientM String 
getUser = client (Proxy :: _ GetUser)

main :: Effect Unit 
main = launchAff_ do 
  result <- runClientM getUser "http://localhost:3000"
  Console.logShow result