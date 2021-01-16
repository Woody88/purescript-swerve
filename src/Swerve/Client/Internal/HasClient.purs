module Swerve.Client.Internal.HasClient where

import Prelude 
import Data.Either 
import Data.Symbol
import Data.Variant (Variant)
import Network.Wai (Request(..), Response(..))
import Swerve.API.Alternative (type (:<|>), (:<|>))
import Swerve.API.ContentType (class MimeUnrender, mimeUnrender)
import Swerve.API.Sub 
import Swerve.API.Verb (Verb)
import Swerve.Client.Internal.Eval (Client, lift)
import Swerve.Client.Internal.Response 
import Swerve.Client.Internal.RunClient (class RunClient, runRequest, throwClientError)
import Type.Proxy (Proxy(..))
import Type.RowList

class HasClient m api where
  clientWithRoute :: Proxy m -> Proxy api -> Request -> Client m api

instance _hasClientAlt :: 
  ( HasClient m a
  , HasClient m b
  ) => HasClient m (a :<|> b) where 
  clientWithRoute pm _ req = lift $ clientWithRoute pm (Proxy :: _ a) req :<|> clientWithRoute pm (Proxy :: _ b) req

instance _hasClientVerb :: 
  ( AsResponse cts rl r  
  , RowToList r rl 
  , RunClient m 
  ) => HasClient m (Verb method cts r) where
  clientWithRoute pm _ req = 
    lift $ runRequest req >>= (either throwClientError pure <<< mkResponse ctypesP rlP)
    where 
      ctypesP = Proxy :: _ cts 
      rlP     = Proxy :: _ rl 

instance _hasClientPath :: 
  ( IsSymbol path 
  , HasClient m api 
  ) => HasClient m (path :> api) where 
  clientWithRoute pm _ (Request req) = 
    lift $ clientWithRoute pm (Proxy :: _ api) $ Request req { url = req.url <> path }
    where 
      path = "/" <> reflectSymbol (SProxy :: _ path)
