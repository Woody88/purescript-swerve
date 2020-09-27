module Swerve.Utils where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Swerve.API.Header (class ToHeader, toHeader)

queryInfo :: String -> Array (Tuple String String)
queryInfo q = 
  q
  # String.split (Pattern "&")
  # map (splitAt (flip Tuple "") "=")
  where 
    splitAt k p str =
        case String.indexOf (Pattern p) str of
          Just i -> Tuple (String.take i str) (String.drop (i + String.length p) str)
          Nothing -> k str

data HeadersUnfold = HeadersUnfold

instance headersUnfoldUnit ::
  ( IsSymbol sym
  , ToHeader a
  ) => FoldingWithIndex HeadersUnfold (SProxy sym) Unit a (Array (Tuple CaseInsensitiveString String))
  where
  foldingWithIndex _ prop _ a = ((wrap $ reflectSymbol prop) /\ (toHeader a)) : []

instance headersUnfold ::
  ( IsSymbol sym
  , ToHeader a
  ) => FoldingWithIndex HeadersUnfold (SProxy sym) (Array (Tuple CaseInsensitiveString String)) a (Array (Tuple CaseInsensitiveString String))
  where
  foldingWithIndex _ prop hdrs a =  ((wrap $ reflectSymbol prop) /\ (toHeader a)) : hdrs

headersToUnfoldable :: forall r.
  HFoldlWithIndex HeadersUnfold Unit { | r } (Array (Tuple CaseInsensitiveString String)) 
  => { | r } 
  -> (Array (Tuple CaseInsensitiveString String))
headersToUnfoldable r = hfoldlWithIndex HeadersUnfold unit r 