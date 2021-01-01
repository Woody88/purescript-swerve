module Swerve.API.Alternative where 

data Alt a b = Alt a b

infixr 3 type Alt as :<|>
infixr 3 Alt as :<|>