module Swerve.Client.Internal.HasClient where

import Prelude

import Data.Either (either)
import Data.Newtype (wrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Network.Wai (Request(..), Response(..))
import Swerve.API.ContentTypes (class MimeUnrender, mimeUnrender)
import Swerve.API.MediaType (PlainText)
import Swerve.API.Verb (Verb)
import Swerve.Client.Internal.RunClient (class RunClient, runRequest, throwClientError)
import Type.Proxy (Proxy(..))

class HasClient m api a where 
  clientWithRoute :: Proxy api -> Request -> m a 


instance hasClientAPI :: (IsSymbol path, RunClient m, MimeUnrender PlainText String) =>  HasClient m (Verb method status path specs) String where 
  clientWithRoute _ (Request req) = do 
    response <- runRequest $ wrap $ req { url = reflectSymbol (SProxy :: _ path) } 
    case response of 
      ResponseString _ _ str -> either throwClientError pure $ mimeUnrender (Proxy :: _ PlainText)  str
      _ -> throwClientError "Bad Response type"