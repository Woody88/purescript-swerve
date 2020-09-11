module Swerve.Server.Internal.RouterI where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Data.Either (either)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
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
  routerI :: Proxy layout -> handler -> Request -> m Response

instance routerIAlt :: 
  ( RouterI a handlera m 
  , RouterI b handlerb m 
  , Alt m
  ) => RouterI (a :<|> b) (handlera :<|> handlerb ) m where 
  routerI _ (handlera :<|> handlerb) req = routerI (Proxy :: _ a) handlera req  <|> routerI (Proxy :: _ b) handlerb req 

else instance routerINoContent :: 
  ( Router (Verb method S204 path specs) path specs params 
  , Conn (Verb method S204 path specs) params
  , HasResponse (HandlerT (Verb method S204 path specs) m NoContent) params m
  , MonadAff m
  , MonadThrow String m
  ) => RouterI (Verb method S204 path specs) (HandlerT (Verb method S204 path specs) m NoContent) m where 
  routerI specP handler req = do 
    eparams  <- liftAff $ runExceptT $ router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    params   <- either throwError pure eparams
    eHandler <- runHandler params handler req 
    pure $ responseStr noContent204 [] mempty

else instance routerIImpl :: 
  ( Router (Verb method status path specs) path specs params 
  , RowToList specs spcrl 
  , Conn (Verb method status path specs) params
  , HasResponse (HandlerT (Verb method status path specs) m result) params m
  , MonadAff m
  , MonadThrow String m
  ) => RouterI (Verb method status path specs) (HandlerT (Verb method status path specs) m result) m where 
  routerI specP handler req = do 
    eparams  <- liftAff $ runExceptT $ router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    params   <- either throwError pure eparams
    runHandler params handler req