module Swerve.Server.Internal.Response where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, inj)
import Swerve.API.Status (class HasStatus, WithStatus(..))
import Type.Row (class Cons)
import Type.Proxy (Proxy)

respond :: forall a r r' status label. 
  IsSymbol label
  => HasStatus status label 
  => Cons label (WithStatus status a) r r' 
  => Proxy status 
  -> a 
  -> Variant r'
respond p = respond' <<< WithStatus p 

respond' :: forall a r r' label. 
  IsSymbol label
  => HasStatus a label 
  => Cons label a r r' 
  => a 
  -> Variant r'
respond' = inj (SProxy :: _ label)
