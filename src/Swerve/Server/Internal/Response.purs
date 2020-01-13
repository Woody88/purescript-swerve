module Swerve.Server.Internal.Response where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (makeAff)
import Network.HTTP.Types (hContentType, status200, status500)
import Network.Wai (responseStr)
import Network.Wai.Internal (Response)
import Simple.JSON (class WriteForeign, writeJSON)
import Swerve.API.MediaType (JSON)
import Swerve.API.RequestMethod (GetRequest)
import Swerve.Server.Internal.Handler (Handler(..), runHandler)
import Swerve.Server.Internal.Route (Route)
import Type.Data.Row (RProxy(..))
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class HasResponse route handler | route -> handler where 
    toResponse :: Proxy route -> handler -> Effect Response

instance hasResponseGet :: 
    ( WriteForeign resp 
    ) => HasResponse (Route url GetRequest bdy resp JSON props) (Handler route resp) where
    toResponse _ handle = do 
        let headers = [ hContentType /\ "application/json" ]
        resp <- runHandler handle
        case resp of 
            Left l -> pure $ responseStr status500 headers (writeJSON status500)
            Right (r :: resp) -> pure $ responseStr status200 headers (writeJSON r)