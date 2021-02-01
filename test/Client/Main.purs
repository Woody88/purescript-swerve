module Test.Client.Main where 

import Prelude

import Data.Either
import Data.Array.Partial as ArrayP
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.String.Base64 as Base64
import Data.Symbol
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Variant as V 
import Effect (Effect)
import Effect.Aff (launchAff_)
import Network.HTTP.Types (hAuthorization, ok200, unauthorized401)
import Network.Wai as Wai
import Network.Warp.Settings
import Partial.Unsafe (unsafePartial)
import Simple.JSON (writeJSON)
import Swerve.API.Status
import Swerve.API.BasicAuth (BasicAuthData(..))
import Swerve.Client.Internal
import Swerve.Client.Internal as Client
import Swerve.Client.Internal.Auth (AuthenticatedRequest, mkAuthenticatedRequest)
import Swerve.Client.ClientM
import Swerve.Server.Internal.BasicAuth (decodeBAHeader)
import Swerve.Client.Internal.Request (addHeader)
import Test.API
import Test.Auth.Example (AuthGenAPI)
import Test.Auth.Example as TA
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec  

spec :: Spec Unit
spec = do 
  let settings = defaultSettings { port = 0 }

  describe "client" do
    around (withStubbedApi settings apiApp) do
      
        it "gets Person" $ \baseUrl -> do
          let api = client (Proxy :: _ API)
          let getOkResult = V.default (Left "bad status") # V.on (SProxy :: _ "200") (\(WithStatus _ p) -> Right p)
          eRes <- runClientM api.person.jim baseUrl

          (eRes >>= getOkResult) `shouldEqual` (Right jimmy)

        it "handles basic auth request" $ \baseUrl -> do
          let api = client (Proxy :: _ API)
          let getOkResult = V.default (Left "bad status") # V.on (SProxy :: _ "200") (\(WithStatus _ p) -> Right p)
          let woody = "woody"
          let basicAuth = BasicAuthData {username: woody, password: "password" } 
          eRes <- runClientM (api.private.secret basicAuth) baseUrl

          (eRes >>= getOkResult) `shouldEqual` (Right (woody <> " secret"))

    around (withStubbedApi settings TA.app) do
      it "handles generalized auth request" $ \baseUrl -> do
          let api = client (Proxy :: _ AuthGenAPI)
          let getOkResult = V.default (Left "bad status") # V.on (SProxy :: _ "200") (\(WithStatus _ p) -> Right p)
          let (id /\ user) = unsafePartial $ ArrayP.head TA.users 
          let cookie = "swerve-auth-cookie" <> "=" <> id 
          let setCookie = addHeader (wrap "X-Cookie")
          let auth = mkAuthenticatedRequest cookie setCookie
          eRes <- runClientM (api.private auth) baseUrl

          (eRes >>= getOkResult) `shouldEqual` (Right $ "this is a secret: " <> TA.account user)
