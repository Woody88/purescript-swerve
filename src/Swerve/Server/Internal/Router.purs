module Swerve.Server.Internal.Router where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect (Effect)
import Effect.Exception (throw)
import Network.Wai (Application)
import Network.Wai.Internal (Request(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Swerve.API.RequestMethod (GetRequest)
import Swerve.Server.Internal.Handler (Handler(..))
import Swerve.Server.Internal.ParseCapture (class ParseCapture, parseCapture)
import Swerve.Server.Internal.Response (class HasResponse, toResponse)
import Swerve.Server.Internal.Route (Route)
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy)
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEq
import Type.Proxy (Proxy(..))

class RoutesHandlers (routesL :: RL.RowList) (routes :: # Type) (handlers :: # Type) | routesL -> routes handlers where  
  matchRoutesImpl :: RLProxy routesL -> RProxy routes -> Record handlers -> Application

instance routesHandlersNil :: RoutesHandlers RL.Nil routes handlers where 
  matchRoutesImpl _ _ _ _ _ = pure unit 

instance routesHandlersCons :: 
  ( RoutesHandlers rtail routes handlers
  , IsSymbol name 
  , Row.Cons name handler handlers' handlers 
  , Row.Cons name route routes' routes 
  , RegisterHandler route handler 
  ) => RoutesHandlers (RL.Cons name route rtail) routes handlers where 
  matchRoutesImpl _ _ handlers app = do 
    let nameP = SProxy :: SProxy name
    registerHandlerImpl (Proxy :: Proxy route) (Record.get nameP handlers) app

class RegisterHandler route handler | route  -> handler where 
  registerHandlerImpl :: Proxy route -> handler -> Application

instance registerHandlerGet :: 
  ( ParseCapture path prams
  , IsSymbol path
  , TypeEquals { params :: { | prams}} (Record conn) 
  , TypeEquals (Route path GetRequest bdy resp ctype props) route
  , HasResponse route (Handler route resp) 
  ) => RegisterHandler route (Record conn -> Handler route resp) where
  registerHandlerImpl route handler (Request req) respond = case parseCapture (SProxy :: SProxy path) req.rawPathInfo of 
    Left l -> throw "couldn't match path"
    Right (p  :: Record prams)  -> do 
      let (conn :: Record conn) = (TypeEq.to $ { params: p })
          handle = handler conn
      respond =<< toResponse route handle 