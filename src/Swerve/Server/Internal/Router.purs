module Swerve.Internal.Router where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, throwError)
import Control.Monad.Reader (runReaderT)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Foreign.Object as Object
import Network.HTTP.Types (hAccept, hContentType, noContent204, notAcceptable406)
import Network.Wai (Request, Response, responseStr)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Extra (class MapRecord, mapRecord)
import Swerve.API.Combinators (type (:<|>), (:<|>))
import Swerve.API.ContentTypes (class AllCTRender, AcceptHeader(..), NoContent(..), handleAcceptH)
import Swerve.API.Spec (Header'(..), ReqBody'(..), Resource'(..))
import Swerve.API.StatusCode (class HasStatus, S204, StatusP(..), toStatus)
import Swerve.API.Verb (class ReflectMethod, Verb, VerbP(..), reflectMethod)
import Swerve.Internal.ParseSpec (class ParseConnSpec, parseConnSpec)
import Swerve.Server.Internal.Handler (Handler(..), toParams)
import Swerve.Server.Internal.ParseBody (class ParseBody, class ParseResource, parseBody)
import Swerve.Server.Internal.ParseCapture (class ParseCapture, parseCapture)
import Swerve.Server.Internal.ParseHeader (class ToHeader, toHeader)
import Swerve.Server.Internal.ParseMethod (methodCheck)
import Swerve.Server.Internal.ParseQuery (class ParseQuery, parseQuery)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, PCons, PNil, PProxy(..), QueryVar, Segment, kind PList)
import Swerver.Server.Internal.Conn (class Conn, ConnectionRow)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous as Row


class RouterI layout handler | layout -> handler, handler -> layout  where
  routerI :: Proxy layout -> handler -> Request -> ExceptT String Aff Response


instance routerIAlt :: 
  ( RouterI a handlera 
  , RouterI b handlerb 
  ) => RouterI (a :<|> b) (handlera :<|> handlerb ) where 
  routerI _ (handlera :<|> handlerb) req = routerI (Proxy :: _ a) handlera req  <|> routerI (Proxy :: _ b) handlerb req 

else instance routerINoContent :: 
  ( Router (Verb method S204 path specs) path specs params 
  , Conn (Verb method S204 path specs) params
  , HasResponse' (Handler (Verb method S204 path specs) NoContent) params
  ) => RouterI (Verb method S204 path specs) (Handler (Verb method S204 path specs) NoContent)  where 
  routerI specP handler req = do 
    params   <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    eHandler <- runHandler' params handler req 
    pure $ responseStr noContent204 [] mempty

else instance routerIImpl :: 
  ( Router (Verb method status path specs) path specs params 
  , RowToList specs spcrl 
  , Conn (Verb method status path specs) params
  , ParseResource spcrl resp ctype
  , AllCTRender ctype resp 
  , HasResponse' handler params  
  ) => RouterI (Verb method status path specs) handler  where 
  routerI specP handler req = do 
    params <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
    runHandler' params handler req  


class HasResponse' handler params where 
  runHandler' :: { | params } -> handler -> Request -> ExceptT String Aff Response 

instance hasResponseHeader ::
  ( Conn (Verb method status path specs) params
  , RowToList specs spcrl 
  , ParseResource spcrl resp ctype
  , AllCTRender ctype resp 
  , Row.Homogeneous hdrs' String 
  , Row.HomogeneousRowList hdrl' String
  , RowToList hdrs' hdrl' 
  , RowToList hdrs hdrl  
  , ToHeader a
  , HasStatus status
  , MapRecord hdrl hdrs a String () hdrs'
  ) => HasResponse' (Handler (Verb method status path specs) (Header' { | hdrs}   resp)) params where 
  runHandler' params (Handler handler) req = do
    let verbP = Proxy :: _ (Verb method status path specs)
    (Header' hdrs resource) <- runReaderT handler (toParams verbP params)
    let accH = getAcceptHeader req
        hdrs' = headersToUnfoldable hdrs
    case handleAcceptH (Proxy :: _ ctype) accH resource of
      Nothing -> do
        throwError notAcceptable406.message
      Just (ct /\ body) -> do 
        pure $ responseStr (toStatus (StatusP :: _ status)) ([hContentType /\ ct] <> map (\t -> (wrap $ fst t) /\ snd t) hdrs') body

else instance hasResponse :: 
  ( Conn (Verb method status path specs) params
  , RowToList specs spcrl 
  , ParseResource spcrl resp ctype
  , AllCTRender ctype resp 
  , HasStatus status
  ) => HasResponse' (Handler (Verb method status path specs) resp) params where 
  runHandler' params (Handler handler) req = do
    let verbP = Proxy :: _ (Verb method status path specs)
    resource <- runReaderT handler (toParams verbP params)
    let accH = getAcceptHeader req
    case handleAcceptH (Proxy :: _ ctype) accH resource of
      Nothing -> do
        throwError notAcceptable406.message
        -- resp $ responseStr notAcceptable406 [] notAcceptable406.message
      Just (ct /\ body) -> do 
        pure $ responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body

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

getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader = AcceptHeader <<< fromMaybe ct_wildcard <<< Map.lookup hAccept <<< Map.fromFoldable <<< _.headers <<< unwrap

ct_wildcard :: String
ct_wildcard = "*" <> "/" <> "*"

class SubRecord (base :: # Type) (sub :: # Type) (conn :: # Type) | base conn -> sub where
  subrecord :: {|base} -> {|conn}

instance subrecordI ::
  ( RowToList sub rl
  , SubRecordRL base rl () conn
  ) => SubRecord base sub conn where
  subrecord base =
    Builder.build (subrecordRL base (RLProxy :: _ rl)) {}

class SubRecordRL (base :: # Type) (rl :: RowList) (from :: # Type) (to :: # Type) | rl -> from to where
  subrecordRL :: {|base} -> RLProxy rl -> Builder {|from} {|to}

instance subrecordRLNil :: SubRecordRL base RL.Nil () () where
  subrecordRL _ _ = identity

instance subrecordRLConsResource :: SubRecordRL base (RL.Cons "resource" (Resource' v ctype) tl) from from where
  subrecordRL b rl = identity

else instance subrecordRLConsBody ::
         ( SubRecordRL base tl from from'
         , Symbol.Append "body" "" k
         , IsSymbol k
         , Row.Cons k v _b base
         , Row.Cons k v from' to
         , Row.Lacks k from'
         ) => SubRecordRL base (RL.Cons "body" (ReqBody' v ctype) tl) from to where
  subrecordRL base _ = hBuilder <<< tlBuilder
    where
      tlBuilder = subrecordRL base (RLProxy :: _ tl)
      sp = SProxy :: _ k
      v = Record.get sp base
      hBuilder = Builder.insert (SProxy :: _ k) v

else instance subrecordRLCons ::
         ( SubRecordRL base tl from from'
         , IsSymbol k
         , Row.Cons k v _b base
         , Row.Cons k v from' to
         , Row.Lacks k from'
         ) => SubRecordRL base (RL.Cons k v tl) from to where
  subrecordRL base _ = hBuilder <<< tlBuilder
    where
      tlBuilder = subrecordRL base (RLProxy :: _ tl)
      sp = SProxy :: _ k
      v = Record.get sp base
      hBuilder = Builder.insert (SProxy :: _ k) v

class Router verb (url :: Symbol) (specs :: # Type) (conn :: # Type) | url specs -> conn where
  router :: Proxy verb -> SProxy url -> RProxy specs -> String -> Request -> ExceptT String Aff {|conn}

instance routerImpl ::
  ( Parse url xs
  , RowToList specs spcl
  , ParsePath xs specs () cap () qry 
  , ParseConnSpec spcl () hdr 
  , ParseBody specs (ReqBody' bdy ctype)
  , SubRecord (ConnectionRow cap qry hdr bdy) specs conn
  , ReflectMethod method
  ) => Router (Verb method status url specs) url specs conn where
  router vp _ rp url req = do 
    _ <- except $ methodCheck (reflectMethod (VerbP :: _ method)) req
    { capture, query, header } <- conns <$> bldrs <*> bldrs2
    (ReqBody' (body :: bdy))  <- parseBody rp req 
    pure $ subrecord { capture, query, header, body }
      
    where
      bldrs = parsePath (PProxy :: _ xs) (RProxy :: _ specs) url req

      bldrs2 = parseConnSpec (RLProxy :: _ spcl) req

      conns p s =  
        { capture: Builder.build p.capture {}
        , query: Builder.build p.query {}
        , header: Builder.build s.header {}
        }

class ParsePath 
  (xs :: PList) (specs :: # Type) 
  (cfrom :: # Type) (cto :: # Type) 
  (qfrom :: # Type) (qto :: # Type) 
  |xs -> cfrom cto qfrom qto where
  parsePath :: 
    PProxy xs 
    -> RProxy specs 
    -> String 
    -> Request 
    -> ExceptT String Aff { capture :: Builder {| cfrom } {| cto }
                          , query   :: Builder {| qfrom } {| qto }
                          }
  
instance parsePathNil :: ParsePath PNil specs cto cto qto qto where
  parsePath _ _ _ _ =
    pure 
      { capture: identity
      , query: identity
      }

instance parsePathCapture ::
  ( IsSymbol var
  , Row.Cons "capture" { | cspcs } _spcs spcs
  , ParseCapture var cspcs cfrom' cto 
  , ParsePath tail spcs cfrom cfrom' qfrom qto
  ) => ParsePath (PCons (CaptureVar var) tail) spcs cfrom cto qfrom qto where
  parsePath _ specs url url' = do
    let { after, before } = String.splitAt 1 url 
        (Tuple var tail)  = case flip String.splitAt after <$> readIndex after of 
                              Nothing                               -> (Tuple after "")
                              Just {after: after', before: before'} -> Tuple before' after'

    capture  <- except $ parseCapture varP (RProxy :: RProxy cspcs) var
    conn     <- parsePath (PProxy :: _ tail) (RProxy :: _ spcs) tail url'

    pure $ { capture: capture <<< conn.capture
           , query: conn.query
           }
    where
      readIndex u = String.indexOf (Pattern "/") u <|> String.indexOf (Pattern "?") u
      varP = (SProxy :: _ var)
      captureP = SProxy :: SProxy "capture"

instance parsePathQuery ::
  ( IsSymbol var
  , Row.Cons "query" { | qspcs } _spcs spcs
  , Row.Cons var vtype r qspcs
  , ParseQuery var vtype qfrom' qto 
  , ParsePath tail spcs cfrom cto qfrom qfrom'
  ) => ParsePath (PCons (QueryVar var) tail) spcs cfrom cto qfrom qto where
  parsePath _ specs url url' = do
    
    let { after, before } = String.splitAt 1 url 
        queryMap = after
                    # String.split (Pattern "&") 
                    # mapMaybe (\d -> flip String.splitAt d <$> String.indexOf (Pattern "=") d)
                    # map (\{after: v, before: k} -> Tuple k (String.drop 1 v))
                    # Map.fromFoldable

        (Tuple val tail) = case Map.pop (reflectSymbol $ varP) queryMap of 
                            Just (Tuple v qmap) -> Tuple v (joinMap qmap)
                            Nothing             -> Tuple "" (joinMap queryMap)
            
    query  <- except $ parseQuery varP (Proxy :: Proxy vtype) val
    conn   <- parsePath (PProxy :: _ tail) (RProxy :: _ spcs) tail url'

    pure $ { query: query <<< conn.query
           , capture: conn.capture
           }
    where
      joinMap = foldrWithIndex (\k v  b -> "&" <> k <> "=" <> v <> b) ""
      readIndex u  = String.indexOf (Pattern "&") u 
      queryValue q = String.drop 1 <<<  maybe "" _.after <<< map (flip String.splitAt q) $ String.indexOf (Pattern "=") q
      varP = (SProxy :: _ var)
      captureP = SProxy :: SProxy "query"

instance parseSegmentRoot :: ParsePath tail specs cfrom cto qfrom qto => ParsePath (PCons (Segment "") tail) specs cfrom cto qfrom qto where 
  parsePath _ specs url req 
    | Just tail <- String.stripPrefix (Pattern "/") url = parsePath (PProxy :: _ tail) specs url req
    | otherwise = throwError $ "could not parse root from '" <> (_.url $ unwrap req) <> "'" 

else instance parseSegment :: 
  ( IsSymbol seg 
  , ParsePath tail specs cfrom cto qfrom qto
  ) => ParsePath (PCons (Segment seg) tail) specs cfrom cto qfrom qto where 
  parsePath _ specs url req = do
    let { after, before } = String.splitAt 1 url 
    case String.stripPrefix (Pattern segment) after of  
      Just tail -> parsePath (PProxy :: _ tail) specs tail req
      otherwise -> throwError $ "could not parse segment '" <> segment <> "' from '" <> (_.url $ unwrap req) <> "'" 
    where 
      segment = reflectSymbol (SProxy :: _ seg)