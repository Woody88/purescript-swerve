module Swerve.Server.Internal.RouterI where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Network.HTTP.Types (noContent204)
import Network.Wai (Request, Response, responseStr)
import Prim.RowList (class RowToList)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.ContentTypes (class AllCTRender, NoContent)
import Swerve.API.StatusCode (S204)
import Swerve.API.Verb (Verb)
import Swerve.Internal.Router (class Router, router)
import Swerve.Server.Internal.Handler (Handler)
import Swerve.Server.Internal.Resource (class Resource)
import Swerve.Server.Internal.Response (class HasResponse, runHandler)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy(..))

class RouterI layout handler | layout -> handler, handler -> layout  where
  routerI :: Proxy layout -> handler -> Request -> ExceptT String Aff Response

instance routerIAlt :: 
  ( RouterI a handlera 
  , RouterI b handlerb 
  ) => RouterI (a :<|> b) (handlera :<|> handlerb ) where 
  routerI _ (handlera :<|> handlerb) req = routerI (Proxy :: _ a) handlera req  <|> routerI (Proxy :: _ b) handlerb req 

else instance routerINoContent :: 
  ( Router (Verb method S204 path specs) path specs params 
  , Conn (Verb method S204 path specs) params
  , HasResponse (Handler (Verb method S204 path specs) NoContent) params
  ) => RouterI (Verb method S204 path specs) (Handler (Verb method S204 path specs) NoContent)  where 
  routerI specP handler req = do 
    params   <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    eHandler <- runHandler params handler req 
    pure $ responseStr noContent204 [] mempty

else instance routerIImpl :: 
  ( Router (Verb method status path specs) path specs params 
  , RowToList specs spcrl 
  , Conn (Verb method status path specs) params
  , HasResponse handler params  
  ) => RouterI (Verb method status path specs) handler  where 
  routerI specP handler req = do 
    params <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    runHandler params handler req 