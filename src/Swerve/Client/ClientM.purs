module Swerve.Client.ClientM where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Alt (class Alt)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.HTTP.Method (fromString)
import Data.HTTP.Method as Method
import Data.Newtype (wrap, unwrap)
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Data.Variant
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream as Stream
import Swerve.Client.Internal.RunClient (class RunClient, throwClientError)
import Swerve.Client.Internal.Request

-- These two type synonyms will eventually change 
type BaseUrl = String
type ClientError = String 

type ClientM a = ClientM' (Variant a) 

newtype ClientM' a = ClientM (ReaderT BaseUrl (ExceptT ClientError Aff) a)

derive newtype instance functorClientM' :: Functor ClientM' 
derive newtype instance applyClientM' :: Apply ClientM'
derive newtype instance applicativeClientM' :: Applicative ClientM'
derive newtype instance bindClientM' :: Bind ClientM'
derive newtype instance monadClientM' :: Monad ClientM'
derive newtype instance monadEffectClientM' :: MonadEffect ClientM'
derive newtype instance monadAffClientM' :: MonadAff ClientM'
derive newtype instance monadReaderClientM' :: MonadReader String ClientM'
derive newtype instance monadAskClientM' :: MonadAsk String ClientM' 
derive newtype instance monadThrowClientM' :: MonadThrow String ClientM'
derive newtype instance altClientM' :: Alt ClientM' 

runClientM :: forall a. ClientM a -> BaseUrl -> Aff (Either ClientError (Variant a))
runClientM (ClientM cm) baseUrl = runExceptT $ flip runReaderT baseUrl $ cm

instance runClientClientM :: RunClient ClientM' where 
  throwClientError = throwError
  runRequest req = do 
    baseUrl <- ask
    let url     = baseUrl <> req.url
    let hdrs    = map (\(Tuple ck v) -> RequestHeader (unwrap ck) v) req.headers
    let method  = Method.fromString req.method 
    let content = (RequestBody.string <<< convertBody <<< fst) <$> req.body
    
    -- TODO: withCredentials = true 
    result <- liftAff $ AX.request $ AX.defaultRequest { method = method, url = url, responseFormat = ResponseFormat.string, headers = hdrs, content = content }
    
    case result of 
      Left e -> throwClientError $ AX.printError e
      Right res -> do 
        let status  = { code: unwrap res.status, message: res.statusText }
        let headers = map (\(ResponseHeader n v) ->  wrap n /\ v) res.headers 
        pure $ { status, headers, body: res.body, httpVersion: req.httpVersion }

    where   
      convertBody (RequestBodyStr str) = str