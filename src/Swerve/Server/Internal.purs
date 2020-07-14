module Swerve.Server.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..))
import Effect.Class.Console as Console
import Network.HTTP.Media as Media
import Network.HTTP.Types (hAccept, hContentType, status200)
import Network.Wai (Application, responseStr)
import Network.Wai.Internal (Request(..), Response)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Swerve.API.ContentTypes (class Accepts, class AllMime, allMime, contentType)
import Swerve.API.Context (Context(..))
import Swerve.API.MediaType (JSON, JSON')
import Swerve.API.RequestMethod (GetRequest)
import Swerve.Server.Internal.Handler (Handler)
import Swerve.Server.Internal.ParseCapture (class ParseCapture, parseCapture)
import Swerve.Server.Internal.Response (class HasResponse, class HasResponse', err400Response, toResponse, toResponse')
import Swerve.Server.Internal.Route (Route, RouteResult(..))
import Swerve.Server.Internal.RouterApplication (RoutingApplication)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEq
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type Connection bdy prams
  = ( body   :: bdy
    , params :: Record prams
    )

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

instance registerHandlerWithCapture :: 
  ( ParseCapture path prams
  , IsSymbol path
  , TypeEquals { params :: { | prams}} (Record params') 
  , TypeEquals (Route path GetRequest bdy resp ctype spec) route
  , RL.RowToList spec specL
  , HasSpec route specL params' conn
  , Row.Union conn trash to 
  , HasResponse route (Handler route resp) 
  ) => RegisterHandler route ({ | conn} -> Handler route resp) where
  registerHandlerImpl route handler rq@(Request req) respond = do 
    case parseCapture (SProxy :: SProxy path) req.rawPathInfo of 
      Left l -> respond $ NotMatched
      Right (p  :: Record prams)  -> do
        case runSpec route (RLProxy :: RLProxy specL) rq of 
           Left l -> respond $ NotMatched
           Right c -> do 
            let (conn :: Record params') = (TypeEq.to $ { params: p })
                m  = Builder.build c conn
                handle = handler m
            respond <<< Matched =<< toResponse route handle 

-- | This will build the connection for handlers.
-- | The purpose of the connection is to inject rows that might be useful
-- | i.e: request body, params, query params, header, etc...
class ConnBuilder route (specL :: RL.RowList) (from :: # Type) (to :: # Type) | specL -> from to where 
  connBuilder :: Proxy route -> RLProxy specL -> Context -> Either String (Builder { | from } { | to })

-- | ReqFilter will act like a circuit breaker, if the implementation of any spec
-- | does not return a Unit, this means that the request cannot be accepted
class ReqFilter route (specL :: RL.RowList) where 
  reqFilter :: Proxy route -> RLProxy specL -> Context -> Either String Unit 


class HasSpec route (specL :: RL.RowList) (from :: # Type) (to :: # Type) | specL -> from to where 
  runSpec :: Proxy route -> RLProxy specL -> Request -> Either String (Builder { | from } { | to })

instance hasSpecContentType :: 
  ( Accepts ctype
  , HasSpec route rtail from to
  ) => HasSpec route (RL.Cons "content-type" ctype rtail) from to where 
  runSpec route specs rq@(Request req) = do 
    let headers = Map.fromFoldable $ req.requestHeaders
        ct = show $ contentType (Proxy :: Proxy ctype)
    case Map.lookup hContentType headers of 
      Just ctt -> runSpec route (RLProxy :: RLProxy rtail) rq
      Nothing  -> Left $ "content-type invalid"

instance hasSpecAccept :: 
  ( Accepts ctype
  , HasSpec route rtail from to
  ) => HasSpec route (RL.Cons "accept" ctype rtail) from to where 
  runSpec route specs rq@(Request req) = do 
    let headers = Map.fromFoldable $ req.requestHeaders
        ct = show $ contentType (Proxy :: Proxy ctype)
    case Map.lookup hContentType headers of 
      Just ctt -> runSpec route (RLProxy :: RLProxy rtail) rq
      Nothing  -> Left $ "accept invalid"

instance hasSpecNil :: Row.Union conn trash (Connection bdy prams) =>  HasSpec route RL.Nil conn conn where 
  runSpec route specs rq@(Request req) = pure identity

-- class HasReqSpec route (spec :: RL.RowList) where 
--     reqSpec :: Proxy route -> Request -> RLProxy spec -> Either String (Proxy route)

-- instance hasReqSpecNil :: HasReqSpec route RL.Nil where 
--     reqSpec route _ _ = pure route 

-- instance hasReqSpecContent :: 
--     ( HasReqSpec route rtail 
--     , AllMime ctype
--     ) => HasReqSpec route (RL.Cons "content-type" ctype rtail) where 
--     reqSpec route rq@(Request req) _ = do 
--         let lookupHeader = flip Map.lookup $ Map.fromFoldable req.requestHeaders 
--             mCtHeader = lookupHeader $ String.toLower hContentType
--             options = allMime (Proxy :: Proxy ctype)
--         (acceptContent $ mCtHeader >>= Media.matchAccept options) >>= \routeResult -> reqSpec routeResult rq (RLProxy :: RLProxy rtail)
--         where 
--             acceptContent mCtype
--                 | Just ctype <- mCtype = Right route 
--                 | otherwise = Left "content type header not found"


-- ct_wildcard :: String
-- ct_wildcard = "*" <> "/" <> "*" 