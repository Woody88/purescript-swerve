module Test.Server.General where 

import Prelude 

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Effect.Aff (launchAff_)
import Network.HTTP.Types (ok200)
import Network.Wai as Wai
import Network.Wai.Internal
import Test.API.Composition as Composition 
import Test.API.CustomMonad as CustomMonad 
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "general" do 
  it "handles api composition" do 
    let 
      request = wrap $ _ { pathInfo = [ "user" ] } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Composition.app request responseFn

  it "handles custom monad" do 
    let 
      request = wrap $ _ { pathInfo = [ "test" ] } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ CustomMonad.app request responseFn