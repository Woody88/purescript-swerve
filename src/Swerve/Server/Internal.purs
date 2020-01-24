module Swerve.Server.Internal where

import Prelude

import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Network.HTTP.Types (hAccept, hContentType, status200)
import Network.Wai (Application, responseStr)
import Network.Wai.Internal (Request(..), Response)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Swerve.API.MediaType (JSON, JSON')
import Swerve.API.RequestMethod (GetRequest)
import Swerve.Server.Internal.Handler (Handler)
import Swerve.Server.Internal.ParseCapture (class ParseCapture, parseCapture)
import Swerve.Server.Internal.Response (class HasResponse, err400Response, toResponse)
import Swerve.Server.Internal.Route (Route, RouteResult(..))
import Swerve.Server.Internal.RouterApplication (RoutingApplication)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEq
import Type.Proxy (Proxy(..))

data Server (api :: # Type) = Server 

class HasServer api handlers | api -> handlers where
    route :: Server api -> Record handlers -> RoutingApplication

class RoutesHandlers (routesL :: RL.RowList) (routes :: # Type) (handlers :: # Type) | routesL -> routes handlers where  
  matchRoutesImpl :: RLProxy routesL -> RProxy routes -> Record handlers -> RoutingApplication

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
  registerHandlerImpl :: Proxy route -> handler -> RoutingApplication

instance registerHandlerGet :: 
  ( ParseCapture path prams
  , IsSymbol path
  , TypeEquals { params :: { | prams}} (Record conn) 
  , TypeEquals (Route path GetRequest bdy resp ctype spec) route
  , RL.RowToList spec specL
  , HasReqSpec route specL
  , HasResponse route (Handler route resp) 
  ) => RegisterHandler route (Record conn -> Handler route resp) where
  registerHandlerImpl route handler rq@(Request req) respond = do 
    Console.logShow req.requestHeaders
    case parseCapture (SProxy :: SProxy path) req.rawPathInfo of 
        Left l -> respond $ NotMatched
        Right (p  :: Record prams)  -> do
            let (conn :: Record conn) = (TypeEq.to $ { params: p })
                handle = handler conn
            case reqSpec route rq (RLProxy :: RLProxy specL) of 
                Left l -> respond $ NotMatched
                Right _ -> respond <<< Matched =<< toResponse route handle 

class HasReqSpec route (spec :: RL.RowList) where 
    reqSpec :: Proxy route -> Request -> RLProxy spec -> Either String (Proxy route)

instance hasReqSpecNil :: HasReqSpec route RL.Nil where 
    reqSpec route _ _ = pure route 

instance hasReqSpecAcceptJson :: 
    ( HasReqSpec route rtail 
    ) => HasReqSpec route (RL.Cons "accept" JSON' rtail) where 
    reqSpec route rq@(Request req) _ = do 
        let lookupHeader = flip Map.lookup $ Map.fromFoldable req.requestHeaders 
        (acceptContent $ lookupHeader $ String.toLower hAccept) >>= \routeResult -> reqSpec routeResult rq (RLProxy :: RLProxy rtail)
        where 
            acceptContent mCtype
                | Just ctype <- mCtype
                , ctype == "application/json" = Right route 
                | otherwise = Left "content type header not found"



        