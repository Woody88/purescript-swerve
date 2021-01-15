module Swerve.Client.Internal.Eval where 

data Client (m :: Type -> Type) (api :: Type)

class EvalClient (a :: Type) (b :: Type) | a -> b