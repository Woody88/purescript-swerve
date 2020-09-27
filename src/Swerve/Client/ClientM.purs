module Swerve.Client.ClientM where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Network.Wai (responseStr)
import Swerve.Client.Internal.RunClient (class RunClient, throwClientError)

-- These two type synonyms will eventually change 
type BaseUrl = String
type ClientError = String 

newtype ClientM a = ClientM (ReaderT BaseUrl (ExceptT ClientError Aff) a)

derive newtype instance functorClientM :: Functor ClientM 
derive newtype instance applyClientM :: Apply ClientM
derive newtype instance applicativeClientM :: Applicative ClientM
derive newtype instance bindClientM :: Bind ClientM
derive newtype instance monadClientM :: Monad ClientM
derive newtype instance monadEffectClientM :: MonadEffect ClientM
derive newtype instance monadAffClientM :: MonadAff ClientM
derive newtype instance monadReaderClientM :: MonadReader String ClientM
derive newtype instance monadAskClientM :: MonadAsk String ClientM 
derive newtype instance monadThrowClientM :: MonadThrow String ClientM
derive newtype instance altClientM :: Alt ClientM 

runClientM :: forall a. ClientM a -> BaseUrl -> Aff (Either ClientError a)
runClientM (ClientM cm) baseUrl = runExceptT $ flip runReaderT baseUrl $ cm

instance runClientClientM :: RunClient ClientM where 
  throwClientError = throwError
  runRequest req = do 
    baseUrl <- ask
    let url = String.joinWith "/" [baseUrl, ( _.url $ unwrap req)]
    result <- liftAff $ AX.request $ AX.defaultRequest { url = url, responseFormat = ResponseFormat.string }
    case result of 
      Left e -> throwClientError $ AX.printError e
      Right resp -> pure $ responseStr { code: unwrap resp.status, message: resp.statusText } [] resp.body
  