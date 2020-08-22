module Swerve.Server.Internal.ParseQuery where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy)

class ReadQuery a where 
  readQuery :: String -> Maybe a 

instance readQueryString :: ReadQuery String where 
  readQuery x = Just x 

instance readQueryInt :: ReadQuery Int where 
  readQuery = Int.fromString
  
class ParseQuery (var :: Symbol) vtype (qfrom :: # Type) (qto :: # Type) | var vtype -> qfrom qto where 
  parseQuery :: SProxy var -> Proxy vtype -> String -> Either String (Builder { | qfrom } { | qto })

instance parseQueryImplMaybe :: 
  ( IsSymbol var
  , ReadQuery vtype
  , Row.Cons var (Maybe vtype) qfrom qto
  , Row.Lacks var qfrom
  ) => ParseQuery var (Maybe vtype) qfrom qto where 
  parseQuery _ _ val = do 
    pure $ Builder.insert (SProxy :: _ var) $ readQuery val

else instance parseQueryImpl :: 
  ( IsSymbol var
  , ReadQuery vtype 
  , Row.Cons var vtype qfrom qto
  , Row.Lacks var qfrom
  ) => ParseQuery var vtype qfrom qto where 
  parseQuery _ _ val = do 
    case readQuery val of 
      Nothing -> Left $ "query could not be parsed. " <> (reflectSymbol (SProxy :: _ var)) <> " " <> val
      Just (v :: vtype) -> do 
        let cBuilder = Builder.insert (SProxy :: _ var) v 
        pure cBuilder 


