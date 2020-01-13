module Swerve.Server.Internal.Router where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect (Effect)
import Effect.Exception (throw)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Swerve.API.MediaType (kind MediaType)
import Swerve.API.RequestMethod (GetRequest, kind RequestMethod)
import Swerve.Server.Internal.ParseCapture (class ParseCapture, parseCapture)
import Swerve.Server.Internal.RoutinApplication (RoutingApplication)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEq
import Type.Proxy (Proxy(..))

data Route (path :: Symbol) (method :: RequestMethod) body resp (ctype :: MediaType) (props :: # Type) 

class RoutesHandlers (routesL :: RL.RowList) (routes :: # Type) (handlers :: # Type) | routesL -> routes handlers where  
  matchRoutesImpl :: RLProxy routesL -> RProxy routes -> Record handlers -> String -> Effect Unit 

instance routesHandlersNil :: RoutesHandlers RL.Nil routes handlers where 
  matchRoutesImpl _ _ _ _ = pure unit 

instance routesHandlersCons :: 
  ( RoutesHandlers rtail routes handlers
  , IsSymbol name 
  , Row.Cons name handler handlers' handlers 
  , Row.Cons name route routes' routes 
  , RegisterHandler route handler 
  ) => RoutesHandlers (RL.Cons name route rtail) routes handlers where 
  matchRoutesImpl _ _ handlers path = do 
    let nameP = SProxy :: SProxy name
    registerHandlerImpl (Proxy :: Proxy route) (Record.get nameP handlers) path

class RegisterHandler route handler | route  -> handler where 
  registerHandlerImpl :: Proxy route -> handler -> String -> Effect Unit 

instance registerHandlerGet :: 
  ( ParseCapture path prams
  , IsSymbol path
  , TypeEquals { params :: { | prams}} (Record conn) 
  ) => RegisterHandler (Route path GetRequest bdy resp ctype props) (Record conn -> Effect Unit) where
  registerHandlerImpl _ handler path = case parseCapture (SProxy :: SProxy path) path of 
    Left l -> throw "couldn't match path"
    Right (p  :: Record prams)  -> do 
      let (conn :: Record conn) = (TypeEq.to $ { params: p })
      handler conn