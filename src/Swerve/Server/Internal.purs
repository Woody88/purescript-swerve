module Swerve.Server.Internal where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Network.HTTP.Types (internalServerError500)
import Network.Wai (Application, responseStr)
import Prim.RowList (class RowToList)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.ContentTypes (NoContent)
import Swerve.API.StatusCode (S204)
import Swerve.API.Verb (Verb)
import Swerve.Server.Internal.Handler (HandlerT(..))
import Swerve.Server.Internal.RouterI (class RouterI, routerI)
import Type.Proxy (Proxy(..))

class HasServer layout handler m | layout -> handler, handler -> layout where 
  route :: Proxy layout -> (m ~> Aff) -> handler -> Application 

instance hasVerbAlt :: 
  ( RouterI a handlera m
  , RouterI b handlerb m
  , MonadAff m
  ) => HasServer (a :<|> b) (handlera :<|> handlerb ) m where 
  route _ runH (handlera :<|> handlerb) req resp = do 
    let handler = routerI (Proxy :: _ a) runH handlera req  <|> routerI (Proxy :: _ b) runH handlerb req 
    eHandler <- runExceptT handler
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response

else instance hasVerbNoContent :: 
  ( RouterI (Verb method S204 path specs) (HandlerT (Verb method S204 path specs) (ExceptT String m) NoContent) m
  , MonadAff m
  ) => HasServer (Verb method S204 path specs) (HandlerT (Verb method S204 path specs) (ExceptT String m) NoContent)  m where 
  route specP runH handler req resp = do 
    eHandler <- runExceptT $ routerI specP runH handler req 
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response

else instance hasVer2 :: 
  ( RouterI (Verb method status path specs) handler m
  , RowToList specs spcrl 
  , MonadAff m
  ) => HasServer (Verb method status path specs) handler m where 
  route specP runH handler req resp = do 
    eHandler <- runExceptT $ routerI specP runH handler req 
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response