module Swerve.Server.Internal.Response where

-- import Prelude
-- import Swerve.API.ContentTypes

-- import Data.Either (Either(..))
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.String as String
-- import Data.Tuple (fst, snd)
-- import Data.Tuple.Nested ((/\))
-- import Effect (Effect)
-- import Effect.Aff (makeAff)
-- import Effect.Exception (throw)
-- import Network.HTTP.Types (hAccept, hContentType, status200, status400, status500)
-- import Network.Wai (Request(..), Response)
-- import Prim.Row as Row
-- import Prim.RowList as RL
-- import Simple.JSON (class WriteForeign, writeJSON)
-- import Swerve.API.MediaType (JSON)
-- import Swerve.API.RequestMethod (GetRequest)
-- import Swerve.Server.Internal.Handler (Handler, runHandler)
-- import Swerve.Server.Internal.Route (Route)
-- import Type.Data.Row (RProxy)
-- import Type.Data.RowList (RLProxy(..))
-- import Type.Proxy (Proxy(..))

-- class HasResponse route handler | route -> handler where 
--     toResponse :: Proxy route -> handler -> Effect Response

-- instance hasResponseGet :: 
--     ( WriteForeign resp 
--     ) => HasResponse (Route url GetRequest bdy resp JSON props) (Handler route resp) where
--     toResponse _ handle = do 
--         let headers = [ hContentType /\ "application/json" ]
--         resp <- runHandler handle
--         case resp of 
--             Left l -> pure $ responseStr status500 headers (writeJSON status500)
--             Right (r :: resp) -> pure $ responseStr status200 headers (writeJSON r)


-- class HasResponse' route handler (specs :: RL.RowList) | route -> handler where 
--     toResponse' :: RLProxy specs -> Proxy route -> Request -> handler -> Effect Response

-- instance hasResponse :: 
--     ( AllCTRender ctype resp
--     ) => HasResponse' (Route url method bdy resp ct props) (Handler route resp) (RL.Cons "accept" ctype tail) where
--     toResponse' pspecs _ (Request req) handle = do 
--         let 
--             lookupHeader = flip Map.lookup $ Map.fromFoldable req.requestHeaders 
--             mCtHeader = AcceptHeader $ fromMaybe "*/*" $ lookupHeader $ String.toLower hAccept

--         resp <- runHandler handle
--         case resp of 
--             Left l -> pure $ responseStr status500 [] status500.message
--             Right (r :: resp) -> do 
--                 case handleAcceptH (Proxy :: Proxy ctype) mCtHeader r of 
--                     Nothing -> throw "fatal error"
--                     Just accept -> pure $ responseStr status200 [hContentType /\ fst accept] (snd accept)

-- -- need to find a better approach
-- err400Response :: Response 
-- err400Response = responseStr status400 [ hContentType /\ "application/json" ] (writeJSON status400)