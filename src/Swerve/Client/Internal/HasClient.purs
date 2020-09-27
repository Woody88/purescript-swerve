module Swerve.Client.Internal.HasClient where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Network.HTTP.Types as H
import Network.Wai (Request(..), Response(..))
import Prim.Row as Row
import Record as Record
import Swerve.API.Capture (Capture)
import Swerve.API.Combinators (type (:>))
import Swerve.API.ContentTypes (class MimeUnrender, mimeUnrender)
import Swerve.API.Resource (Resource)
import Swerve.API.Verb (class ReflectMethod, Verb, reflectMethod)
import Swerve.Client.Internal.RunClient (class RunClient, runRequest, throwClientError)
import Swerve.Server.Internal.HasConn (class HasConn)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, Segment)
import Swerve.Server.InternalPathSub (class PathSub, PathEnd)
import Type.Proxy (Proxy(..))

class HasClient :: forall a. Type -> Row Type -> (a -> Type) -> a -> Constraint
class HasClient api conn m a | api -> conn where 
  clientWithRoute :: Proxy api -> Record conn -> Request -> m a  

instance hasClientVerb
  :: ( Parse path plist
     , PathSub plist api' 
     , HasConn api () conn
     , Client api' api conn m a 
     , ReflectMethod method ) 
  =>  HasClient (Verb method status path :> api) conn m a where 
  clientWithRoute _ conn (Request req) = clientRoute (Proxy :: _ api') (Proxy :: _ api) conn newReq 
    where 
      method' = fromMaybe req.method (H.parseMethod $ reflectMethod (Proxy :: _ method))
      newReq  = Request (req { method = method' })

class Client :: forall a. Type -> Type -> Row Type -> (a -> Type) -> a -> Constraint
class Client pathapi api conn m a | api -> conn where 
  clientRoute :: Proxy pathapi -> Proxy api -> Record conn -> Request -> m a 

instance hasClientResource :: (RunClient m, MimeUnrender ctype a) =>  Client PathEnd (Resource a ctype) conn m a where 
  clientRoute _ _ _ req = do 
    response <- runRequest req 
    case response of 
      ResponseString _ _ str -> either throwClientError pure $ mimeUnrender (Proxy :: _ ctype) str
      _ -> throwClientError "Bad Response type"

instance hasClientCapture 
  :: ( Client pathapi api (capture :: Record ctypes | conn) m a 
     , Row.Cons cvar t r' ctypes 
     , Show t 
     , IsSymbol cvar) 
  =>  Client (CaptureVar cvar :> pathapi) (Capture cvar t :> api) (capture :: Record ctypes | conn) m a where 
  clientRoute _ _ conn (Request req) = clientRoute (Proxy :: _ pathapi) (Proxy :: _ api) conn newReq 
    where 
      cvarP    = (SProxy :: _ cvar)
      capture = Record.get (SProxy :: _ "capture") conn 
      cval    = show $ Record.get cvarP capture
      url' = String.joinWith "/"  [req.url, cval ]
      newReq  = Request (req { url = url' })

instance hasClientSegment 
  :: ( Client pathapi api conn m a 
     , IsSymbol seg ) 
  =>  Client (Segment seg :> pathapi) api conn m a where 
  clientRoute _ api conn (Request req) = clientRoute (Proxy :: _ pathapi) api conn newReq 
    where 
      segment = reflectSymbol (SProxy :: _ seg)
      url' = String.joinWith "/"  $ Array.filter (\v -> (not $ String.null v) && (not $ eq v "/") ) [req.url, segment]
      newReq  = Request (req { url =  url' })