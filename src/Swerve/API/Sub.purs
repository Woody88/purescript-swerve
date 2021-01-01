module Swerve.API.Sub where

data Sub :: forall k. k -> Type -> Type
data Sub a b

infixr 4 type Sub as :>