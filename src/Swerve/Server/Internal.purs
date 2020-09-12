module Swerve.Server.Internal where

import Prelude

import Control.Monad.Except (class MonadError)
import Data.Either (Either(..))
import Effect.Aff (Aff, try)
import Network.HTTP.Types (internalServerError500, notFound404)
import Network.Wai (Application, responseStr)
import Swerve.Server.Internal.Response (SwerveResponse(..))
import Swerve.Server.Internal.RouterI (class RouterI, routerI)
import Type.Proxy (Proxy)

class HasServer api handler m where 
  route :: Proxy api -> (forall a. m a -> Aff (Either String a)) -> handler -> Application 

instance hasServerAlt :: 
  ( RouterI api handler m
  , Monad m
  , MonadError String m
  ) => HasServer api handler m where 
  route api runM handler req resp = do 
    result <- runM do 
      r <- try $ routerI api handler req
      case r of 
        Left _ -> pure $ Rsp $ responseStr notFound404 [] mempty
        Right rsp -> pure rsp  

    case result of
      Left e    -> resp $ responseStr internalServerError500 [] e
      Right rsp -> case rsp of 
        Rsp response -> resp response
        Raw app      -> app req resp