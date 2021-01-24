module Test.Auth where 

import Prelude

import Data.Array.Partial as ArrayP
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Network.HTTP.Types (ok200, forbidden403)
import Network.Wai as Wai
import Network.Wai.Internal
import Partial.Unsafe (unsafePartial)
import Test.Auth.Example as TA
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec  


spec :: Spec Unit
spec = describe "generalized-auth" do

  it "should unauthorize" do 
    let 
      request = wrap $ _ { pathInfo = [ "private", "secret" ] } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` forbidden403) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ TA.app request responseFn

  it "should authorize private endpoint and return user website" do 
    let 
      accKey =  fst $ unsafePartial $ ArrayP.head TA.users 
      cookie = "swerve-auth-cookie" <> "=" <> accKey
      request = wrap $ _ { pathInfo = [ "private", "secret" ], headers = [ Tuple (wrap "Cookie") cookie ] } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ TA.app request responseFn