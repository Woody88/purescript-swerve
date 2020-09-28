module Swerve.API.Combinators 
    ( Alt(..)
    , type (:<|>)
    , (:<|>)
    , Sub(..)
    , type (:>)
    ) where 

data Alt a b = Alt a b 

infixr 7 type Alt as :<|>
infixr 7 Alt as :<|>

data Sub :: forall k. k -> Type -> Type
data Sub (a :: k) b 

infixr 4 type Sub as :>