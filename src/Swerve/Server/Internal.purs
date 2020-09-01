module Swerve.Server.Internal where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff as Aff
import Effect.Aff as Error
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Foreign.Object as Object
import Network.HTTP.Types (hAccept, hContentType, internalServerError500, noContent204, notAcceptable406, ok200)
import Network.Wai (Application, Request(..), responseStr)
import Prim.RowList (class RowToList)
import Record.Extra (class MapRecord, mapRecord)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.ContentTypes (class Accepts, class AllCTRender, class MimeRender, AcceptHeader(..), NoContent, PlainText, handleAcceptH)
import Swerve.API.Spec (Header'(..))
import Swerve.API.StatusCode (class HasStatus, S200, S204, StatusP(..), toStatus)
import Swerve.API.Verb (Verb, VerbP(..))
import Swerve.Internal.Router (class Router, class RouterI, router, routerI)
import Swerve.Server.Internal.Handler (Handler(..), toParams)
import Swerve.Server.Internal.ParseBody (class ParseBodyCT, class ParseResource)
import Swerve.Server.Internal.ParseHeader (class ToHeader, toHeader)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Data.Row (RProxy(..))
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous as Row

class HasServer layout handler | layout -> handler, handler -> layout where 
  route :: Proxy layout -> handler -> Application 

class HasResponse handler params where 
  runHandler :: { | params } -> handler ->  Application 

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
  , HasResponse handler params  
  ) => HasServer (Verb method status path specs) handler  where 
  route specP handler req resp = do 
    eHandler <- runExceptT $ routerI specP handler req 
    case eHandler of 
      Left e -> resp $ responseStr internalServerError500 [] e
      Right response -> resp response

-- Start 

-- instance hasVerbAlt :: 
--   ( HasServer a handlera 
--   , HasServer b handlerb 
--   ) => HasServer (a :<|> b) (handlera :<|> handlerb ) where 
--   route _ (handlera :<|> handlerb) req res = route (Proxy :: _ a) handlera req res <|> route (Proxy :: _ b) handlerb req res

-- else instance hasVerbNoContent :: 
--   ( Router (Verb method S204 path specs) path specs params 
--   , Conn (Verb method S204 path specs) params
--   ) => HasServer (Verb method S204 path specs) (Handler (Verb method S204 path specs) NoContent)  where 
--   route specP handler = noContentRouter specP handler (SProxy :: _ path) (RProxy :: _ specs) 

-- else instance hasVer2 :: 
--   ( Router (Verb method status path specs) path specs params 
--   , RowToList specs spcrl 
--   , Conn (Verb method status path specs) params
--   , ParseResource spcrl resp ctype
--   , AllCTRender ctype resp 
--   , HasResponse handler params  
--   ) => HasServer (Verb method status path specs) handler  where 
--   route specP handler = router' specP handler (SProxy :: _ path) (RProxy :: _ specs) 

-- instance hasResponseHeader ::
--   ( Conn (Verb method status path specs) params
--   , RowToList specs spcrl 
--   , ParseResource spcrl resp ctype
--   , AllCTRender ctype resp 
--   , Row.Homogeneous hdrs' String 
--   , Row.HomogeneousRowList hdrl' String
--   , RowToList hdrs' hdrl' 
--   , RowToList hdrs hdrl  
--   , ToHeader a
--   , HasStatus status
--   , MapRecord hdrl hdrs a String () hdrs'
--   ) => HasResponse (Handler (Verb method status path specs) (Header' { | hdrs}   resp)) params where 
--   runHandler params (Handler handler) req resp = do
--     let verbP = Proxy :: _ (Verb method status path specs)
--     eHandler <- runExceptT $ runReaderT handler (toParams verbP params)
--     case eHandler of 
--       Left e2 -> do 
--         resp $ responseStr internalServerError500 [] mempty
--       Right (Header' hdrs resource) -> do 
--         let accH = getAcceptHeader req
--             hdrs' = headersToUnfoldable hdrs
--         case handleAcceptH (Proxy :: _ ctype) accH resource of
--           Nothing -> do
--             resp $ responseStr notAcceptable406 [] notAcceptable406.message
--           Just (ct /\ body) -> do 
--             resp $ responseStr (toStatus (StatusP :: _ status)) ([hContentType /\ ct] <> map (\t -> (wrap $ fst t) /\ snd t) hdrs') body

-- else instance hasResponse :: 
--   ( Conn (Verb method status path specs) params
--   , RowToList specs spcrl 
--   , ParseResource spcrl resp ctype
--   , AllCTRender ctype resp 
--   , HasStatus status
--   ) => HasResponse (Handler (Verb method status path specs) resp) params where 
--   runHandler params (Handler handler) req resp = do
--     let verbP = Proxy :: _ (Verb method status path specs)
--     eHandler <- runExceptT $ runReaderT handler (toParams verbP params)
--     case eHandler of 
--       Left e2 -> do 
--         resp $ responseStr internalServerError500 [] mempty
--       Right resource -> do 
--         let accH = getAcceptHeader req
--         case handleAcceptH (Proxy :: _ ctype) accH resource of
--           Nothing -> do
--             resp $ responseStr notAcceptable406 [] notAcceptable406.message
--           Just (ct /\ body) -> do 
--             resp $ responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body

--- End
routerHeaders :: forall path specs params method status resp spcrl hdrs ctype.
  Router (Verb method status path specs) path specs params 
  => Conn (Verb method status path specs) params
  => ParseResource spcrl resp ctype
  => RowToList specs spcrl 
  => HasResponse (Handler (Verb method status path specs) (Header' { | hdrs } resp)) params 
  => Proxy (Verb method status path specs) 
  -> Handler (Verb method status path specs) (Header' { | hdrs } resp)
  -> SProxy path 
  -> RProxy specs
  -> Application
routerHeaders verbP handler pathP specsP req resp = do 
  Console.log "in"
  matchedRoute <- runExceptT $ router verbP pathP specsP (_.url $ unwrap req) req 
  case matchedRoute of 
    Left e -> resp $ responseStr internalServerError500 [] mempty
    Right params -> runHandler params handler req resp

headersToUnfoldable :: forall r r' rl rl' a.
  Row.Homogeneous r' String 
  => Row.HomogeneousRowList rl' String
  => RowToList r' rl' 
  => RowToList r rl  
  => ToHeader a
  => MapRecord rl r a String () r' 
  => Record r 
  -> Array (String /\ String)
headersToUnfoldable = Object.toUnfoldable <<<  Object.fromHomogeneous <<< mapRecord (toHeader)

router' :: forall path specs params method status handler resp spcrl ctype.
  Router (Verb method status path specs) path specs params 
  => Conn (Verb method status path specs) params
  => RowToList specs spcrl 
  => ParseResource spcrl resp ctype
  => AllCTRender ctype resp 
  => HasResponse handler params 
  => Proxy (Verb method status path specs) 
  -> handler
  -> SProxy path 
  -> RProxy specs
  -> Application
router' verbP handler pathP specsP req resp = do 
  Console.log "in2"
  matchedRoute <- runExceptT $ router verbP pathP specsP (_.url $ unwrap req) req 
  case matchedRoute of 
    Left e -> resp $ responseStr internalServerError500 [] e
    Right params -> runHandler params handler req resp 

noContentRouter :: forall path specs params method.
  Router (Verb method S204 path specs) path specs params 
  => Conn (Verb method S204 path specs) params
  => Proxy (Verb method S204 path specs) 
  -> Handler (Verb method S204 path specs) NoContent
  -> SProxy path 
  -> RProxy specs
  -> Application
noContentRouter verbP (Handler handler) pathP specsP req resp = do 
  matchedRoute <- runExceptT $ router verbP pathP specsP (_.url $ unwrap req) req 
  case matchedRoute of 
    Left e   -> do 
      resp $ responseStr internalServerError500 [] e
    Right params -> do 
      eHandler <- runExceptT $ runReaderT handler (toParams verbP params)
      case eHandler of 
        Left e2 -> do 
          resp $ responseStr internalServerError500 [] mempty
        Right str -> resp $ responseStr noContent204 [] mempty

getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader = AcceptHeader <<< fromMaybe ct_wildcard <<< Map.lookup hAccept <<< Map.fromFoldable <<< _.headers <<< unwrap

ct_wildcard :: String
ct_wildcard = "*" <> "/" <> "*"









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