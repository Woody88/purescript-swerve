module Test.BasicAuth where 

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Network.HTTP.Types (hAuthorization)
import Network.Wai as Wai
import Swerve.API.BasicAuth (BasicAuthData(..))
import Server.Internal.BasicAuth (decodeBAHeader)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec  

spec :: Spec Unit
spec = describe "basic-auth" do
  it "decode's basic authorization header" do
    let request = wrap $ _ { body = Nothing, headers = [Tuple hAuthorization "Basic aGVsbG86d29ybGQ="]  } $ unwrap Wai.defaultRequest
        basicAuthData = Just $ BasicAuthData {username: "hello", password: "world"} 
    (decodeBAHeader request) `shouldEqual` basicAuthData