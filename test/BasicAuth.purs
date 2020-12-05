module Test.BasicAuth where 

-- import Prelude

-- import Data.Array.Partial as ArrayP
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (wrap, unwrap)
-- import Data.String.Base64 as Base64
-- import Data.Tuple (Tuple(..))
-- import Effect (Effect)
-- import Effect.Aff (launchAff_)
-- import Network.HTTP.Types (hAuthorization, ok200, unauthorized401)
-- import Network.Wai as Wai
-- import Partial.Unsafe (unsafePartial)
-- import Simple.JSON (writeJSON)
-- import Swerve.API.BasicAuth (BasicAuthData(..))
-- import Swerve.Server.Internal.BasicAuth (decodeBAHeader)
-- import Test.BasicAuth.Example as TBA
-- import Test.Spec (Spec, describe, it)
-- import Test.Spec.Assertions (fail, shouldEqual)
-- import Test.Spec.Reporter.Console (consoleReporter)
-- import Test.Spec.Runner (runSpec)

-- main :: Effect Unit
-- main = launchAff_ $ runSpec [consoleReporter] spec  

-- spec :: Spec Unit
-- spec = describe "basic-auth" do
--   it "decode's basic authorization header" do
--     let request = wrap $ _ { headers = [Tuple hAuthorization "Basic aGVsbG86d29ybGQ="]  } $ unwrap Wai.defaultRequest
--         basicAuthData = Just $ BasicAuthData {username: "hello", password: "world"} 
--     (decodeBAHeader request) `shouldEqual` basicAuthData

--   it "decode's encoded basic authorization header" do
--     let 
--       user = unsafePartial $ ArrayP.head TBA.users
--       website = TBA.website user 
--       userpass = Base64.encodeUrl (TBA.username user <> ":" <> TBA.pass user)
--       request = wrap $ _ { headers = [Tuple hAuthorization ("Basic " <> userpass)]  } $ unwrap Wai.defaultRequest
--       basicAuthData = Just $ BasicAuthData {username: TBA.username user, password: TBA.pass user} 

--     (decodeBAHeader request) `shouldEqual` basicAuthData

--   it "should unauthorize" do 
--     let 
--       request = wrap $ _ { pathInfo = [ "mysite" ], headers = [Tuple hAuthorization "Basic aGVsbG86d29ybGQ="] } $ unwrap Wai.defaultRequest
--       responseFn (Wai.ResponseString status headers message) = status `shouldEqual` unauthorized401
--       responseFn _ = fail "fail"

--     TBA.app request responseFn

--   it "should authorize and return user website" do 
--     let 
--       user = unsafePartial $ ArrayP.head TBA.users
--       website = TBA.website user 
--       userpass = Base64.encodeUrl (TBA.username user <> ":" <> TBA.pass user)
--       request = wrap $ _ { pathInfo = [ "mysite" ], headers = [Tuple hAuthorization ("Basic " <> userpass)] } $ unwrap Wai.defaultRequest
--       responseFn (Wai.ResponseString status headers message) = do
--         status `shouldEqual` ok200
--         message `shouldEqual` (writeJSON website)
--       responseFn _ = fail "fail"

--     TBA.app request responseFn