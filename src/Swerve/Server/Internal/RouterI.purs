module Swerve.Server.Internal.RouterI where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Network.HTTP.Types (noContent204)
import Network.Wai (Request, Response, responseStr)
import Prim.RowList (class RowToList)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.ContentTypes (NoContent)
import Swerve.API.StatusCode (S204)
import Swerve.API.Verb (Verb)
import Swerve.Internal.Router (class Router, router)
import Swerve.Server.Internal.Handler (HandlerT)
import Swerve.Server.Internal.Response (class HasResponse, runHandler)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy(..))

class RouterI layout handler m | layout -> handler, handler -> layout  where
  routerI :: Proxy layout -> (m ~> Aff) -> handler -> Request -> ExceptT String Aff Response

instance routerIAlt :: 
  ( RouterI a handlera m 
  , RouterI b handlerb m 
  , Monad m
  ) => RouterI (a :<|> b) (handlera :<|> handlerb ) m where 
  routerI _ runH (handlera :<|> handlerb) req = routerI (Proxy :: _ a) runH handlera req  <|> routerI (Proxy :: _ b) runH handlerb req 

else instance routerINoContent :: 
  ( Router (Verb method S204 path specs) path specs params 
  , Conn (Verb method S204 path specs) params
  , HasResponse (HandlerT (Verb method S204 path specs) m NoContent) params m
  ) => RouterI (Verb method S204 path specs) (HandlerT (Verb method S204 path specs) m NoContent) m where 
  routerI specP runH handler req = do 
    params   <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    eHandler <- pure $ runH $ runHandler params handler req 
    pure $ responseStr noContent204 [] mempty

else instance routerIImpl :: 
  ( Router (Verb method status path specs) path specs params 
  , RowToList specs spcrl 
  , Conn (Verb method status path specs) params
  , HasResponse (HandlerT (Verb method status path specs) m result) params m
  ) => RouterI (Verb method status path specs) (HandlerT (Verb method status path specs) m result) m where 
  routerI specP runH handler req = do 
    params <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    lift $ runH (runHandler params handler req)