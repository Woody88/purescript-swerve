module Swerve.Server.Internal where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Except (ExceptT)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Network.HTTP.Types (internalServerError500)
import Network.Wai (Application, responseStr)
import Prim.RowList (class RowToList)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.ContentTypes (NoContent)
import Swerve.API.StatusCode (S204)
import Swerve.API.Verb (Verb)
import Swerve.Server.Internal.Handler (HandlerT)
import Swerve.Server.Internal.RouterI (class RouterI, routerI)
import Type.Proxy (Proxy(..))

class HasServer layout handler m | layout -> handler, handler -> layout where 
  route :: Proxy layout -> (forall a. m a -> Aff (Either String a)) -> handler -> Application 

instance hasVerbAlt :: 
  ( RouterI a handlera m
  , RouterI b handlerb m
  , Alt m
  ) => HasServer (a :<|> b) (handlera :<|> handlerb ) m where 
  route _ runM (handlera :<|> handlerb) req resp = do 
    let handler = routerI (Proxy :: _ a) handlera req  <|> routerI (Proxy :: _ b) handlerb req 
    eHandler <- runM handler
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response

else instance hasVerbNoContent :: 
  ( RouterI (Verb method S204 path specs) (HandlerT (Verb method S204 path specs) m NoContent) m
  ) => HasServer (Verb method S204 path specs) (HandlerT (Verb method S204 path specs) m NoContent)  m where 
  route specP runM handler req resp = do 
    eHandler <- runM $ routerI specP handler req 
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response

else instance hasVer2 :: 
  ( RouterI (Verb method status path specs) (HandlerT (Verb method status path specs) (ExceptT String m) result) m
  , RowToList specs spcrl 
  ) => HasServer (Verb method status path specs) (HandlerT (Verb method status path specs) (ExceptT String m) result) m where 
  route specP runM handler req resp = do 
    eHandler <- runM $ routerI specP handler req 
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response