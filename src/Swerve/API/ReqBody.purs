module Swerve.API.ReqBody where 

data ReqBody :: forall a contentTypes. a -> contentTypes -> Type
data ReqBody a contentTypes 