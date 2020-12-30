module Test.CustomMonad where

-- import Prelude

-- import Control.Monad.Reader.Class (ask)
-- import Control.Monad.Reader (Reader, runReader)
-- import Data.Debug.Eval as D
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (unwrap, wrap)
-- import Data.Tuple (Tuple(..))
-- import Effect (Effect)
-- import Effect.Aff (Aff)
-- import Effect.Aff as Aff
-- import Effect.Class (liftEffect)
-- import Network.HTTP.Types (hAuthorization, hContentType, ok200)
-- import Network.Wai (Application, Response(..), defaultRequest, responseStr) as Wai
-- import Swerve.API(type (:>), Get, JSON, Ok, Raw, _Ok)
-- import Swerve.Server (class HasResp, Response, Server, Server', hoistServer, respond, serve)
-- import Swerve.Server (from) as Server
-- import Test.Stream (newStream)
-- import Type.Proxy (Proxy(..))

-- type ReaderAPI = GetRaw 

-- type GetRaw = "raw" :> Raw
-- type Test = "test" :> Get String JSON  

-- readerApi = Proxy :: Proxy ReaderAPI

-- getInt :: forall rs
--   .  HasResp Ok () rs
--   => Reader String (Response rs String)
-- getInt = do 
--   v <- ask 
--   pure $ respond _Ok v

-- getRaw :: Reader String Wai.Application
-- getRaw = do 
--   v <- ask
--   pure \req send -> send $ Wai.responseStr ok200 [] v

-- readerServer :: Server' ReaderAPI (Reader String)
-- readerServer = Server.from (getRaw) 

-- nt :: Reader String ~> Aff 
-- nt = (pure <<< (flip runReader "hi"))

-- server :: Server ReaderAPI
-- server = hoistServer readerApi nt readerServer 

-- app :: Wai.Application
-- app = serve (Proxy :: _ ReaderAPI) server

-- main :: Effect Unit
-- main = Aff.launchAff_ do 
--   stream <- liftEffect $ newStream "Hello, World!"
--   app (request stream) responseFn
--   where 
--     request s = wrap $ _ { body = Just s,  pathInfo = [ "raw" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple hContentType "text/plain", Tuple hAuthorization "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
--     responseFn (Wai.ResponseString status headers message) = do 
--       liftEffect $ D.eval { status, headers, message }
--     responseFn _ = liftEffect $ D.eval "bad response"