module Swerve.Server.Internal.ParseCapture where

-- Credits to @justinwoo for this module

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (indexOf, splitAt, stripPrefix) as String
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Global as Global
import Prim.Row as Row
import Prim.Symbol as Symbol
import Prim.TypeError as TE
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Format (class Parse, FCons, FNil, FProxy(..), Lit, Var, kind FList)
import Type.Data.Row (RProxy(..))

class ReadCapture a where 
  readCapture :: String -> Maybe a 

instance readCaptureString :: ReadCapture String where 
  readCapture x = Just x 

instance readCaptureInt :: ReadCapture Int where 
  readCapture = Int.fromString
  
class ParseCapture (var :: Symbol) (specs :: # Type) (cfrom :: # Type) (cto :: # Type) | var specs -> cfrom cto where 
  parseCapture :: SProxy var -> RProxy specs -> String -> Either String (Builder { | cfrom } { | cto })

instance parseCaptureImpl :: 
  ( IsSymbol var
  , Row.Cons var vtype r specs
  , ReadCapture vtype 
  , Row.Cons var vtype cfrom cto
  , Row.Lacks var cfrom
  ) => ParseCapture var specs cfrom cto where 
  parseCapture _ _ val = do 
    case readCapture val of 
      Nothing -> Left "capture could not be parsed."
      Just (v :: vtype) -> do 
        let cBuilder = Builder.insert (SProxy :: _ var) v 
        pure cBuilder 