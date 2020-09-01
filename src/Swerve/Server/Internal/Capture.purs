module Swerve.Server.Internal.Capture where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.Row (RProxy)

class ReadCapture a where 
  readCapture :: String -> Maybe a 

instance readCaptureString :: ReadCapture String where 
  readCapture x = Just x 

instance readCaptureInt :: ReadCapture Int where 
  readCapture = Int.fromString
  
class Capture (var :: Symbol) (specs :: # Type) (cfrom :: # Type) (cto :: # Type) | var specs -> cfrom cto where 
  parseCapture :: SProxy var -> RProxy specs -> String -> Either String (Builder { | cfrom } { | cto })

instance parseCaptureImpl :: 
  ( IsSymbol var
  , Row.Cons var vtype r specs
  , ReadCapture vtype 
  , Row.Cons var vtype cfrom cto
  , Row.Lacks var cfrom
  ) => Capture var specs cfrom cto where 
  parseCapture _ _ val = do 
    case readCapture val of 
      Nothing -> Left "capture could not be parsed."
      Just (v :: vtype) -> do 
        let cBuilder = Builder.insert (SProxy :: _ var) v 
        pure cBuilder 