module Swerve.Internal.Router where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), except, throwError)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Wai (Request)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Swerve.API.ContentTypes (PlainText, contentType)
import Swerve.API.Spec (ReqBody'(..))
import Swerve.API.Verb (class ReflectMethod, Verb, VerbP(..), reflectMethod)
import Swerve.Internal.ParseSpec (class ParseConnSpec, parseConnSpec)
import Swerve.Server.Internal.ParseBody (class ParseBody, parseBody)
import Swerve.Server.Internal.ParseCapture (class ParseCapture, parseCapture)
import Swerve.Server.Internal.ParseMethod (methodCheck)
import Swerve.Server.Internal.ParseQuery (class ParseQuery, parseQuery)
import Swerve.Server.Internal.Path (class Parse, CaptureVar, PCons, PNil, PProxy(..), QueryVar, Segment, kind PList)
import Swerver.Server.Internal.Conn (ConnectionRow)
import Type.Data.Row (RProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

type ConnPath cap qry 
  = ( capture :: Record cap 
    , query   :: Record qry 
    )

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

instance subrecordRLCons ::
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
  , ParseBody specs bdy ctype
  , SubRecord (ConnectionRow cap qry hdr bdy ctype) specs conn
  , Row.Union conn attrs_ specs
  , ReflectMethod method
  ) => Router (Verb method status url specs) url specs conn where
  router vp _ rp url req = do 
    _ <- except $ methodCheck (reflectMethod (VerbP :: _ method)) req
    { capture, query, header } <- conns <$> bldrs <*> bldrs2
    (body :: bdy)  <- parseBody rp req 
    let contentType =  Proxy :: _ ctype
    pure $ subrecord { capture, query, header, body, contentType}
      
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