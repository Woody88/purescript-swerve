module Swerve.API.Types where 

data Spec
data Status
data ContentType 
type Headers = Row Type 
type OtherResponse = Row Type 

foreign import data Sub :: forall k. k -> Spec -> Spec 
foreign import data Alt :: Spec -> Spec -> Spec 

infixr 4 type Sub as :>
infixr 4 type Sub as :/

infixr 3 type Alt as :<|>

foreign import data Seg :: Symbol -> Spec 
foreign import data Capture :: Type -> Spec 
foreign import data QueryParam :: Symbol -> Type -> Spec
foreign import data Header :: Symbol -> Type -> Spec 
foreign import data Verb :: Status -> Type -> Headers -> ContentType -> OtherResponse -> Spec