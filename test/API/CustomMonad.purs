module Test.API.CustomMonad where

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
import Network.Wai.Internal
import Swerve.API 
import Swerve.Server
import Swerve.Server (lift) as Server
import Type.Proxy (Proxy(..))

type ReaderApp a = ReaderAppM (ResponseV a)
type ReaderAppM = Reader String 
type ReaderAPI = Record 
  ( test :: "test" :> Get JSON (Ok String + Nil)
  , raw  :: "raw" :> Raw
  )

readerApi = Proxy :: Proxy ReaderAPI

test :: ReaderApp (Ok String + Nil)
test = do 
  v <- ask 
  pure <<< respond (Proxy :: _ Ok') $ v

raw :: ReaderAppM Wai.Application
raw = do 
  v <- ask
  pure \req send -> send $ Wai.responseStr ok200 [] v

readerServer :: ServerT ReaderAPI ReaderAppM
readerServer = Server.lift { test, raw } 

-- | Natural Transformer. Transforms our ReadAppM to a swerve HandlerM.
nt :: ReaderAppM ~> HandlerM
nt = pure <<< (flip runReader "hi")

server :: Server ReaderAPI 
server = hoistServer readerApi nt readerServer 

app :: Wai.Application
app = serve (Proxy :: _ ReaderAPI) server