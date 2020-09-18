module Swerve.Server.Internal.Header where

-- import Prelude

-- import Control.Monad.Except (ExceptT, except)
-- import Data.Either (Either(..))
-- import Data.Int as Int
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (unwrap, wrap)
-- import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
-- import Effect.Aff (Aff)
-- import Network.HTTP.Types (RequestHeaders)
-- import Network.Wai (Request)
-- import Prim.Row as Row
-- import Prim.RowList (class RowToList, kind RowList)
-- import Prim.RowList as RL
-- import Record.Builder (Builder)
-- import Record.Builder as Builder
-- import Swerve.API.Spec (Header'(..))
-- import Type.Data.RowList (RLProxy(..))

-- class Header   
--   (specs :: RowList)
--   (hfrom :: # Type) (hto :: # Type) 
--   |specs -> hfrom hto where 
--   parseHeader :: 
--     RLProxy specs 
--     -> Request
--     -> ExceptT String Aff { header :: Builder {| hfrom } {| hto } }

-- instance headerNil :: Header RL.Nil hto hto where 
--   parseHeader _ req = pure { header: identity }

-- instance headerImpl ::
--   ( RowToList htypes hrl
--   , ParseHeader hrl hfrom' hto 
--   , Header tail hfrom hfrom'
--   ) => Header (RL.Cons "header" { | htypes } tail) hfrom hto where
--   parseHeader _ req = do
--     specs <- parseHeader (RLProxy :: _ tail) req 
--     hdr <- except $ parseHeader' (RLProxy :: _ hrl) (_.headers $ unwrap req) 
--     pure $ { header: hdr <<< specs.header }

-- else instance headers :: Header tail hfrom hto => Header (RL.Cons specs htype tail) hfrom hto where 
--   parseHeader _ req = parseHeader (RLProxy :: _ tail) req
  
-- class ReadHeader a where 
--   readHeader :: String -> Maybe a 

-- instance readHeaderString :: ReadHeader String where 
--   readHeader x = Just x 

-- instance readHeaderInt :: ReadHeader Int where 
--   readHeader = Int.fromString

-- class ToHeader a where 
--   toHeader :: a -> String 

-- instance toHeaderString :: ToHeader String where 
--   toHeader x = x 

-- instance toHeaderInt :: ToHeader Int where 
--   toHeader = show

-- class ParseHeader (hdrl :: RowList) (hfrom :: # Type) (hto :: # Type) | hdrl -> hfrom hto where 
--   parseHeader' :: RLProxy hdrl -> RequestHeaders -> Either String (Builder { | hfrom } { | hto })

-- instance parseHeaderNil :: ParseHeader RL.Nil hto hto where 
--   parseHeader' _ _ = pure identity

-- instance parseHeaderImplMaybe :: 
--   ( IsSymbol var
--   , ReadHeader vtype
--   , Row.Cons var (Maybe vtype) hfrom' hto
--   , Row.Lacks var hfrom'
--   , ParseHeader tail hfrom hfrom'
--   ) => ParseHeader (RL.Cons var (Maybe vtype) tail) hfrom hto where 
--   parseHeader' _ rhdrs = do 
--     let varP = (SProxy :: _ var)
--         hdr = Map.fromFoldable rhdrs
--                     # (readHeader <=< Map.lookup (wrap $ reflectSymbol varP))
--                     # Builder.insert varP

--     hdrs <- parseHeader' (RLProxy :: _ tail) rhdrs 
--     pure $ hdr <<< hdrs

-- else instance parseHeaderImpl :: 
--   ( IsSymbol var
--   , ReadHeader vtype 
--   , Row.Cons var vtype hfrom' hto
--   , Row.Lacks var hfrom'
--   , ParseHeader tail hfrom hfrom'
--   ) => ParseHeader (RL.Cons var vtype tail) hfrom hto where 
--   parseHeader' _ rhdrs = do 
--     let varP = (SProxy :: _ var)
--         mHeader = Map.fromFoldable rhdrs
--                     # (readHeader <=< Map.lookup (wrap $ reflectSymbol varP))

--     hdrs <- parseHeader' (RLProxy :: _ tail) rhdrs 
--     hdr <- case mHeader of 
--             Nothing -> Left $ "header could not be parsed. " <> (reflectSymbol varP) <> "in" <> show rhdrs
--             Just (v :: vtype) -> pure $ Builder.insert varP v 
--     pure (hdr <<< hdrs)

-- withHeader :: forall m headers a. Monad m => { | headers } -> a -> m (Header' { | headers} a)
-- withHeader hdr a = pure $ Header' hdr a