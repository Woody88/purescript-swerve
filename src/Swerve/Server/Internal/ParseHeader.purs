module Swerve.Server.Internal.ParseHeader where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Network.HTTP.Types (RequestHeaders)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

class ReadHeader a where 
  readHeader :: String -> Maybe a 

instance readHeaderString :: ReadHeader String where 
  readHeader x = Just x 

instance readHeaderInt :: ReadHeader Int where 
  readHeader = Int.fromString

class ParseHeader (hdrl :: RowList) (hfrom :: # Type) (hto :: # Type) | hdrl -> hfrom hto where 
  parseHeader :: RLProxy hdrl -> RequestHeaders -> Either String (Builder { | hfrom } { | hto })

instance parseHeaderNil :: ParseHeader RL.Nil hto hto where 
  parseHeader _ _ = pure identity

instance parseHeaderImplMaybe :: 
  ( IsSymbol var
  , ReadHeader vtype
  , Row.Cons var (Maybe vtype) hfrom' hto
  , Row.Lacks var hfrom'
  , ParseHeader tail hfrom hfrom'
  ) => ParseHeader (RL.Cons var (Maybe vtype) tail) hfrom hto where 
  parseHeader _ rhdrs = do 
    let varP = (SProxy :: _ var)
        hdr = Map.fromFoldable rhdrs
                    # (readHeader <=< Map.lookup (wrap $ reflectSymbol varP))
                    # Builder.insert varP

    hdrs <- parseHeader (RLProxy :: _ tail) rhdrs 
    pure $ hdr <<< hdrs

else instance parseHeaderImpl :: 
  ( IsSymbol var
  , ReadHeader vtype 
  , Row.Cons var vtype hfrom' hto
  , Row.Lacks var hfrom'
  , ParseHeader tail hfrom hfrom'
  ) => ParseHeader (RL.Cons var vtype tail) hfrom hto where 
  parseHeader _ rhdrs = do 
    let varP = (SProxy :: _ var)
        mHeader = Map.fromFoldable rhdrs
                    # (readHeader <=< Map.lookup (wrap $ reflectSymbol varP))

    hdrs <- parseHeader (RLProxy :: _ tail) rhdrs 
    hdr <- case mHeader of 
            Nothing -> Left $ "header could not be parsed. " <> (reflectSymbol varP) <> "in" <> show rhdrs
            Just (v :: vtype) -> pure $ Builder.insert varP v 
    pure (hdr <<< hdrs)



-- else instance parseHeaderImpl :: 
--   ( IsSymbol var
--   , ReadHeader vtype 
--   , Row.Cons var vtype qfrom qto
--   , Row.Lacks var qfrom
--   ) => ParseHeader var vtype qfrom qto where 
--   parseHeader _ _ hdrs = do 
--     let varP = (SProxy :: _ var)
--         mHeader = Map.fromFoldable hdrs
--                     # (readHeader <=< Map.lookup (wrap $ reflectSymbol varP))

--     case mHeader of 
--       Nothing -> Left $ "header could not be parsed. " <> (reflectSymbol varP) <> "in" <> show hdrs
--       Just (v :: vtype) -> do 
--         let cBuilder = Builder.insert (SProxy :: _ var) v 
--         pure cBuilder 