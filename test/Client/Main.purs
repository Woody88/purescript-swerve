module Test.Client.Main where 

import Prelude

import Data.Either
import Data.Array.Partial as ArrayP
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.String.Base64 as Base64
import Data.Symbol
import Data.Tuple (Tuple(..))
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
import Swerve.Client.ClientM
import Swerve.Server.Internal.BasicAuth (decodeBAHeader)
import Test.API
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

  around (withStubbedApi settings apiApp) do
    let api = client (Proxy :: _ API)
    describe "client" do
      it "gets Person" $ \baseUrl -> do
        let getOkResult = V.default (Left "bad status") # V.on (SProxy :: _ "200") (\(WithStatus _ p) -> Right p)
        eRes <- runClientM api.person.jim baseUrl

        (eRes >>= getOkResult) `shouldEqual` (Right jimmy)

      it "handles basic request" $ \baseUrl -> do
        let getOkResult = V.default (Left "bad status") # V.on (SProxy :: _ "200") (\(WithStatus _ p) -> Right p)
        let woody = "woody"
        let basicAuth = BasicAuthData {username: woody, password: "password" } 
        eRes <- runClientM (api.private.secret basicAuth) baseUrl

        (eRes >>= getOkResult) `shouldEqual` (Right (woody <> " secret"))
