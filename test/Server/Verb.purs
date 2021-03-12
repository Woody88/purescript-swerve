module Test.Server.Verb where

import Prelude 

import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.String.CodePoints as String 
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect) 
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (ok200, methodNotAllowed405)
import Network.HTTP.Types.Method (StdMethod(..))
import Network.Wai as Wai
import Network.Wai.Internal (ResponseReceived(..))
import Test.API.Verb as Verb 
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Utils (newStream)

spec :: Spec Unit
spec = describe "verbs" do
  it "handles 'GET' method" do 
    let 
      request = wrap $ _ { pathInfo = [ "get" ] } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Verb.app request responseFn

  it "handles 'POST' method" do 
    b <- liftEffect $ newStream "random-body" 
    let 
      request = wrap $ _ { body = Just b, pathInfo = [ "post" ], method = POST, headers = [ hContentType /\ "text/plain" ] } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Verb.app request responseFn

  it "handles 'POST' method with empty body" do 
    let 
      request = wrap $ _ { pathInfo = [ "post-no-body"], method = POST } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Verb.app request responseFn

  it "handles 'PATCH' method" do 
    let 
      request = wrap $ _ { pathInfo = [ "patch" ], method = PATCH } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Verb.app request responseFn

  it "handles 'PUT' method" do 
    let 
      request = wrap $ _ { pathInfo = [ "put" ], method = PUT } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Verb.app request responseFn

  it "handles 'Delete' method" do 
    let 
      request = wrap $ _ { pathInfo = [ "delete" ], method = DELETE } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` ok200) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Verb.app request responseFn

  it "should return method not allowed status method" do 
    let 
      request = wrap $ _ { pathInfo = [ "delete" ] } $ unwrap Wai.defaultRequest
      responseFn (Wai.ResponseString status headers message) = (status `shouldEqual` methodNotAllowed405) *> pure ResponseReceived
      responseFn _ = fail "fail" *> pure ResponseReceived

    void $ Verb.app request responseFn