module Swerve.API.Endpoint where 

data Endpoint :: forall k. Symbol -> k -> Type
data Endpoint name k 