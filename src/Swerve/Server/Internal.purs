module Swerve.Server.Internal where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Network.HTTP.Types (internalServerError500, noContent204)
import Network.Wai (Application, responseStr)
import Swerve.API.ContentTypes (NoContent(..))
import Swerve.API.StatusCode (S204)
import Swerve.API.Verb (GET, Verb)
import Swerve.Internal.ParseRoute (class ParseRoute, parseRoute)
import Swerve.Server.Internal.Handler (Handler(..), toParams)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy)

type ConnectionRow cap qry
  = ( capture :: Record cap
    , query   :: Record qry
    )

class HasServer layout handler | layout -> handler, handler -> layout where 
  route :: Proxy layout -> handler -> Application 

instance hasVerb :: 
  ( ParseRoute path specs params 
  , Conn (Verb method S204 path specs) params
  ) => HasServer (Verb method S204 path specs) (Handler (Verb method S204 path specs) NoContent)  where 
  route specP (Handler handler) req resp = case parseRoute (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req)  of 
    Left e     -> do 
      Console.logShow e
      resp $ responseStr internalServerError500 [] mempty
    Right params -> do 
      eHandler <- runExceptT $ runReaderT handler (toParams specP params)
      case eHandler of 
        Left e2 -> resp $ responseStr internalServerError500 [] mempty
        Right str -> resp $ responseStr noContent204 [] mempty

-- instance hasVerb :: 
--   ( ParseRoute path specs params 
--   , Conn (Verb GET status path specs) params
--   , Show a 
--   ) => HasServer (Verb GET status path specs) (Handler (Verb GET status path specs) a)  where 
--   route specP (Handler handler) url req resp = case parseRoute (SProxy :: _ path) (RProxy :: _ specs) url of 
--     Left e     -> throw e 
--     Right params -> do 
--       eHandler <- runExceptT $ runReaderT handler (toParams specP params)
--       case eHandler of 
--         Left e2 -> throw e2
--         Right str -> pure $ show str











-- class Router layout handler | layout -> handler where
--   route :: Proxy layout -> handler -> Application

-- class HasContent (specs ::  # Type) ct | specs -> ct

-- instance aHasContent :: Row.Cons "content" ct t specs => HasContent specs ct

-- instance routePath :: Router (Verb v path specs) handler where 
--   route _ handler = \req res -> do 
--     pure unit 

-- instance routerGet :: (MkConnection specs conn, HasContent specs ct) => Router (Verb GET path specs) (Handler' (Verb GET path specs) conn ct) where 
--   route _ (Handler handler) = \req res -> do it 
--       Just conn -> do
--         eResult <- runExceptT $ runReaderT handler conn
--         case eResult of 
--           Right r -> res $ responseStr ok200 [] "Hello, World!" 
--           Left e -> pure unit


-- swerve :: forall layout handler. Router layout handler => Proxy layout -> handler -> Application 
-- swerve = route

-- type Connection bdy prams
--   = ( body   :: bdy
--     , params :: Record prams
--     )


-- class RoutesHandlers (routesL :: RL.RowList) (routes :: # Type) (handlers :: # Type) | routesL -> routes handlers where  
--   matchRoutesImpl :: RLProxy routesL -> RProxy routes -> Record handlers -> RoutingApplication

-- instance routesHandlersNil :: RoutesHandlers RL.Nil routes handlers where 
--   matchRoutesImpl _ _ _ _ _ = pure unit 

-- instance routesHandlersCons :: 
--   ( RoutesHandlers rtail routes handlers
--   , IsSymbol name 
--   , Row.Cons name handler handlers' handlers 
--   , Row.Cons name route routes' routes 
--   , RegisterHandler route handler 
--   ) => RoutesHandlers (RL.Cons name route rtail) routes handlers where 
--   matchRoutesImpl _ _ handlers app = do 
--     let nameP = SProxy :: SProxy name
--     registerHandlerImpl (Proxy :: Proxy route) (Record.get nameP handlers) app

-- class RegisterHandler route handler | route  -> handler where 
--   registerHandlerImpl :: Proxy route -> handler -> RoutingApplication

-- instance registerHandlerWithCapture :: 
--   ( ParseCapture path prams
--   , IsSymbol path
--   , TypeEquals { params :: { | prams}} (Record params') 
--   , TypeEquals (Route path GetRequest bdy resp ctype spec) route
--   , RL.RowToList spec specL
--   , HasSpec route specL params' conn
--   , Row.Union conn trash to 
--   , HasResponse route (Handler route resp) 
--   ) => RegisterHandler route ({ | conn} -> Handler route resp) where
--   registerHandlerImpl route handler rq@(Request req) respond = do 
--     case parseCapture (SProxy :: SProxy path) req.rawPathInfo of 
--       Left l -> respond $ NotMatched
--       Right (p  :: Record prams)  -> do
--         case runSpec route (RLProxy :: RLProxy specL) rq of 
--            Left l -> respond $ NotMatched
--            Right c -> do 
--             let (conn :: Record params') = (TypeEq.to $ { params: p })
--                 m  = Builder.build c conn
--                 handle = handler m
--             respond <<< Matched =<< toResponse route handle 

-- -- | This will build the connection for handlers.
-- -- | The purpose of the connection is to inject rows that might be useful
-- -- | i.e: request body, params, query params, header, etc...
-- class ConnBuilder route (specL :: RL.RowList) (from :: # Type) (to :: # Type) | specL -> from to where 
--   connBuilder :: Proxy route -> RLProxy specL -> Context -> Either String (Builder { | from } { | to })

-- -- | ReqFilter will act like a circuit breaker, if the implementation of any spec
-- -- | does not return a Unit, this means that the request cannot be accepted
-- class ReqFilter route (specL :: RL.RowList) where 
--   reqFilter :: Proxy route -> RLProxy specL -> Context -> Either String Unit 


-- class HasSpec route (specL :: RL.RowList) (from :: # Type) (to :: # Type) | specL -> from to where 
--   runSpec :: Proxy route -> RLProxy specL -> Request -> Either String (Builder { | from } { | to })

-- instance hasSpecBody :: HasSpec route (RL.Cons "content-type" ctype rtail) from to where 
  
-- instance hasSpecContentType :: 
--   ( Accepts ctype
--   , HasSpec route rtail from to
--   ) => HasSpec route (RL.Cons "content-type" ctype rtail) from to where 
--   runSpec route specs rq@(Request req) = do 
--     let headers = Map.fromFoldable $ req.requestHeaders
--         ct = show $ contentType (Proxy :: Proxy ctype)
--     case Map.lookup hContentType headers of 
--       Just ctt -> runSpec route (RLProxy :: RLProxy rtail) rq
--       Nothing  -> Left $ "content-type invalid"

-- instance hasSpecAccept :: 
--   ( Accepts ctype
--   , HasSpec route rtail from to
--   ) => HasSpec route (RL.Cons "accept" ctype rtail) from to where 
--   runSpec route specs rq@(Request req) = do 
--     let headers = Map.fromFoldable $ req.requestHeaders
--         ct = show $ contentType (Proxy :: Proxy ctype)
--     case Map.lookup hContentType headers of 
--       Just ctt -> runSpec route (RLProxy :: RLProxy rtail) rq
--       Nothing  -> Left $ "accept invalid"

-- instance hasSpecNil :: Row.Union conn trash (Connection bdy prams) =>  HasSpec route RL.Nil conn conn where 
--   runSpec route specs rq@(Request req) = pure identity



---- before --- 
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