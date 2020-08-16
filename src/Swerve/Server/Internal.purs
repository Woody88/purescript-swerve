module Swerve.Server.Internal where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.HTTP.Types (ok200)
import Network.Wai (Application, Request(..), responseStr)
import Network.Warp (pathInfo)
import Prim.Row (class Cons)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Record as Record
import Record as Reord
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Builder as Builder
import Swerve.API.Verb (GET, Verb)
import Swerve.Server.Internal.Handler (Handler'(..), Handler)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, PCons, PNil, PPath(..), PProxy(..), QueryVar, Segment, kind PList, kind Path)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (valueFromString)

type ConnectionRow cap qry
  = ( capture :: Record cap
    , query   :: Record qry
    )

class ReadCapture a where
  readCapture :: String -> Maybe a

instance readCaptureString :: ReadCapture String where
  readCapture x = Just x

instance readCaptureInt :: ReadCapture Int where
  readCapture = Int.fromString

class Subrecord (base :: # Type) (sub :: # Type) where
  subrecord :: {|base} -> {|sub}

instance subrecordI ::
  ( RowToList sub rl
  , SubrecordRL base rl () sub
  ) => Subrecord base sub where
  subrecord base =
    Builder.build (subrecordRL base (RLProxy :: _ rl)) {}

class SubrecordRL (base :: # Type) (rl :: RowList) (from :: # Type) (to :: # Type) | rl -> from to where
  subrecordRL :: {|base} -> RLProxy rl -> Builder {|from} {|to}

instance subrecordRLNil :: SubrecordRL base Nil () () where
  subrecordRL _ _ = identity

instance subrecordRLCons ::
         ( SubrecordRL base tl from from'
         , IsSymbol k
         , Row.Cons k v _b base
         , Row.Cons k v from' to
         , Row.Lacks k from'
         ) => SubrecordRL base (Cons k v tl) from to where
  subrecordRL base _ = hBuilder <<< tlBuilder
    where
      tlBuilder = subrecordRL base (RLProxy :: _ tl)
      sp = SProxy :: _ k
      v = Record.get sp base
      hBuilder = Builder.insert (SProxy :: _ k) v

class ParseRoute (url :: Symbol) (specs :: # Type) where
  parseRoute :: SProxy url -> RProxy specs -> String -> Either String {|specs}

instance parseRouteImpl ::
  ( Parse url xs
  , ParsePath xs specs () cap () qry
  , Subrecord (ConnectionRow cap qry) specs
  ) => ParseRoute url specs where
  parseRoute _ _ url = subrecord <$> conn
    where
      bldrs = parsePath (PProxy :: _ xs) (RProxy :: _ specs) url url

      conn :: Either String {|ConnectionRow cap qry}
      conn = bldrs <#> \b ->
        { capture: Builder.build b.capture {}
        , query: Builder.build b.query {}
        }

class ParsePath (xs :: PList) (specs :: # Type) (capfrom :: # Type) (capto :: # Type) (queryfrom :: # Type) (queryto :: # Type) | xs -> capfrom capto queryfrom queryto where
  parsePath :: PProxy xs -> RProxy specs -> String -> String -> Either String { capture :: Builder { | capfrom } { | capto }
          , query :: Builder {|queryfrom} {|queryto}
          }

instance parsePathNil :: ParsePath PNil specs capto capto qryto qryto where
  parsePath _ _ _ _ =
    Right { capture: identity
          , query: identity
          }

instance parseCapture ::
  ( IsSymbol var
  , ReadCapture vtype
  , Row.Cons var vtype _foo ctype
  , Row.Cons "capture" { | ctype } _spcs spcs
  , ParsePath tail spcs capfrom capfrom' qryfrom qryto

  , Row.Cons var vtype capfrom' capto
  , Row.Lacks var capfrom'
  ) => ParsePath (PCons (CaptureVar var) tail) spcs capfrom capto qryfrom qryto where
  parsePath _ specs url url' = case v of
    Just v' ->
      case tl of
        Right r ->
          let h = Builder.insert (SProxy :: _ var) v' :: Builder {|capfrom'} {|capto}
          in Right $ { capture: h <<< r.capture
                     , query: r.query
                     }
        Left e ->
          Left $ "tail gave up: " <> e
    Nothing ->
      Left "giving up"

    where
      hUrl = url -- pretend I'm getting the first segment
      tlUrl = url -- pretend I'm getting the rest

      v :: Maybe vtype
      v = readCapture hUrl -- should really be a segment

      tl :: Either String { capture :: Builder {|capfrom} {|capfrom'}, query :: Builder {|qryfrom} {|qryto} }
      tl = parsePath (PProxy :: _ tail) specs tlUrl url'

instance parseQuery ::
  ( IsSymbol var
  , ReadCapture vtype
  , Row.Cons var vtype _foo qtype
  , Row.Cons "query" { | qtype } _spcs spcs
  , ParsePath tail spcs capfrom capto qryfrom qryfrom'

  , Row.Cons var vtype qryfrom' qryto
  , Row.Lacks var qryfrom'
  ) => ParsePath (PCons (QueryVar var) tail) spcs capfrom capto qryfrom qryto where
  parsePath _ specs url url' = case v of
    Just v' ->
      case tl of
        Right r ->
          let h = Builder.insert (SProxy :: _ var) v' :: Builder {|qryfrom'} {|qryto}
          in Right $ { capture: r.capture
                     , query: h <<< r.query
                     }
        Left e ->
          Left $ "tail gave up: " <> e
    Nothing ->
      Left "giving up"

    where
      hUrl = url -- pretend I'm the url with q param
      tlUrl = url -- pretend I'm the url having removed q param

      v :: Maybe vtype
      v = readCapture hUrl

      tl :: Either String { capture :: Builder {|capfrom} {|capto}, query :: Builder {|qryfrom} {|qryfrom'} }
      tl = parsePath (PProxy :: _ tail) specs tlUrl url'


instance parseSegmentRoot :: ParsePath tail specs capfrom capto qryfrom qryto => ParsePath (PCons (Segment "") tail) specs capfrom capto qryfrom qryto where
  parsePath _ specs url url' = Left "bar"

else instance parseSegment ::
  ( IsSymbol seg
  , ParsePath tail specs capfrom capto qryfrom qryto
  ) => ParsePath (PCons (Segment seg) tail) specs capfrom capto qryfrom qryto where
  parsePath _ specs url url' = Left "zz"

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
