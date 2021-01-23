module Swerve.Client.Internal.HasClient where

import Prelude 
import Data.Either 
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Effect 
import Network.Wai (Request(..), Response(..))
import Node.Stream 
import Swerve.API.Alternative (type (:<|>), (:<|>))
import Swerve.API.Capture 
import Swerve.API.ContentType (class MimeUnrender, mimeUnrender)
import Swerve.API.Header 
import Swerve.API.QueryParam 
import Swerve.API.Sub 
import Swerve.API.Raw 
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

instance _hasClientRaw :: RunClient m => HasClient m Raw where
  clientWithRoute pm _ (Request req) = lift $ \httpMethod -> runRequest $ Request req { method = httpMethod }

instance _hasClientPath :: 
  ( IsSymbol path 
  , HasClient m api 
  ) => HasClient m (path :> api) where 
  clientWithRoute pm _ (Request req) = 
    lift $ clientWithRoute pm (Proxy :: _ api) $ Request req { url = req.url <> path }
    where 
      path = "/" <> reflectSymbol (SProxy :: _ path)

instance _hasClientCapture :: 
  ( IsSymbol name 
  , ToCapture a
  , HasClient m api 
  ) => HasClient m (Capture name a :> api) where 
  clientWithRoute pm _ (Request req) = 
    lift $ \val -> clientWithRoute pm (Proxy :: _ api) $ Request req { url = req.url <> "/" <> toCapture val }

instance _hasClientHeader :: 
  ( IsSymbol name 
  , ToHeader a
  , HasClient m api 
  ) => HasClient m (Header name a :> api) where 
  clientWithRoute pm _ (Request req) = 
    lift $ \val -> do 
      let hname = wrap $ reflectSymbol (SProxy :: _ name)
      let headers' = [ hname /\ toHeader val ] <> req.headers
      clientWithRoute pm (Proxy :: _ api) $ Request req { headers = headers' }

instance _hasClientQueryParam :: 
  ( IsSymbol name 
  , ToQueryParam a
  , HasClient m api 
  ) => HasClient m (QueryParam name a :> api) where 
  clientWithRoute pm _ (Request req) = 
    lift $ \val -> do 
      let qname = reflectSymbol (SProxy :: _ name)
      let queryString' = [ qname /\ (Just $ toQueryParam val) ] <> req.queryString
      clientWithRoute pm (Proxy :: _ api) $ Request req { queryString = queryString' }

-- instance _hasClientReqBody :: 
--   ( IsSymbol name 
--   , MimeRender ct a
--   , HasClient m api 
--   ) => HasClient m (ReqBody ct a :> api) where 
--   clientWithRoute pm _ (Request req) = 
--     lift $ \body -> do 
--       let body' = mimeRender (Proxy :: _ ct) body
           
--       let queryString' = [ qname /\ (Just $ toQueryParam val) ] <> req.queryString
--       clientWithRoute pm (Proxy :: _ api) $ Request req { queryString = queryString' }


foreign import toReadableStream :: String -> Effect (Readable ())