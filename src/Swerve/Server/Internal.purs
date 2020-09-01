module Swerve.Server.Internal where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Network.HTTP.Types (internalServerError500)
import Network.Wai (Application, responseStr)
import Prim.RowList (class RowToList)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.ContentTypes (class AllCTRender, NoContent)
import Swerve.API.StatusCode (S204)
import Swerve.API.Verb (Verb)
import Swerve.Server.Internal.Handler (Handler)
import Swerve.Server.Internal.ParseBody (class ParseResource)
import Swerve.Server.Internal.RouterI (class RouterI, routerI)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Proxy (Proxy(..))

class HasServer layout handler | layout -> handler, handler -> layout where 
  route :: Proxy layout -> handler -> Application 

instance hasVerbAlt :: 
  ( RouterI a handlera 
  , RouterI b handlerb 
  ) => HasServer (a :<|> b) (handlera :<|> handlerb ) where 
  route _ (handlera :<|> handlerb) req resp = do 
    let handler = routerI (Proxy :: _ a) handlera req  <|> routerI (Proxy :: _ b) handlerb req 
    eHandler <- runExceptT handler
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response

else instance hasVerbNoContent :: 
  ( RouterI (Verb method S204 path specs) (Handler (Verb method S204 path specs) NoContent) 
  , Conn (Verb method S204 path specs) params
  ) => HasServer (Verb method S204 path specs) (Handler (Verb method S204 path specs) NoContent)  where 
  route specP handler req resp = do 
    eHandler <- runExceptT $ routerI specP handler req 
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response

else instance hasVer2 :: 
  ( RouterI (Verb method status path specs) handler
  , RowToList specs spcrl 
  , Conn (Verb method status path specs) params
  , ParseResource spcrl resp ctype
  , AllCTRender ctype resp 
  ) => HasServer (Verb method status path specs) handler  where 
  route specP handler req resp = do 
    eHandler <- runExceptT $ routerI specP handler req 
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response