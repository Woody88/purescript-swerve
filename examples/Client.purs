module Examples.Client where

import Prelude 

import Data.Either
import Data.Debug.Eval as D
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Data.Variant 
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.HTTP.Types (hAuthorization, hContentType, ok200)
import Network.Wai (Application, Response(..), defaultRequest, responseStr) as Wai
import Swerve.API
import Swerve.Server 
import Swerve.Server (lift) as Server
import Type.Proxy (Proxy(..))
import Swerve.Client.ClientM (ClientM(..), runClientM)
import Swerve.Client.Internal (client)
import Swerve.Client.Internal as Client 

type User = String

type UserAPI
 = "users" :> Get JSON (Ok User + Nil)


getUser :: ClientM (Ok User + Nil)
getUser = Client.lift $ client (Proxy :: _ UserAPI) 

main :: Effect Unit 
main = launchAff_ do 
  eresult <- runClientM getUser "http://localhost:3000"
  liftEffect $ case eresult of 
    Left e -> D.eval e
    Right v -> match { "200": \resp -> D.eval resp } v
