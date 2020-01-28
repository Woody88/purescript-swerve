module Swerve.API.Combinators 
    ( Alt(..)
    , type (:<|>)
    ) where 

data Alt a b

infixr 7 type Alt as :<|>