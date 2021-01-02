module Test.CustomMonad where

import Prelude

import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader (Reader, runReader)
import Data.Debug.Eval as D
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Network.HTTP.Types (hAuthorization, hContentType, ok200)
import Network.Wai (Application, Response(..), defaultRequest, responseStr) as Wai
import Swerve.API 
import Swerve.Server
import Swerve.Server (lift) as Server
import Test.Stream (newStream)
import Type.Proxy (Proxy(..))

type ReaderApp a = ReaderAppM (ResponseV a)
type ReaderAppM = Reader String 
type ReaderAPI = Test :<|> GetRaw 

type GetRaw = "raw" :> Raw
type Test = "test" :> Get JSON (Ok String + Nil)

readerApi = Proxy :: Proxy ReaderAPI

getInt :: ReaderApp (Ok String + Nil)
getInt = do 
  v <- ask 
  pure <<< respond (Proxy :: _ Ok') $ v

getRaw :: ReaderAppM Wai.Application
getRaw = do 
  v <- ask
  pure \req send -> send $ Wai.responseStr ok200 [] v

readerServer :: ServerT ReaderAPI ReaderAppM
readerServer = Server.lift (getInt :<|> getRaw) 

-- | Natural Transformer. Transforms our ReadAppM to a swerve HandlerM.
nt :: ReaderAppM ~> HandlerM
nt = pure <<< (flip runReader "hi")

server :: Server ReaderAPI 
server = hoistServer readerApi nt readerServer 

app :: Wai.Application
app = serve (Proxy :: _ ReaderAPI) server

main :: Effect Unit
main = Aff.launchAff_ do 
  stream <- liftEffect $ newStream "Hello, World!"
  app (request stream) responseFn
  where 
    request s = wrap $ _ { body = Just s,  pathInfo = [ "test" ], queryString = [ Tuple "maxAge" (Just "30") ], headers = [Tuple hContentType "text/plain", Tuple hAuthorization "Basic d29vZHk6cGFyc3N3b3Jk"]  } $ unwrap Wai.defaultRequest
    responseFn (Wai.ResponseString status headers message) = do 
      liftEffect $ D.eval { status, headers, message }
    responseFn _ = liftEffect $ D.eval "bad response"