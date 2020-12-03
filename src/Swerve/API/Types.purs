module Swerve.API.Types where 

data Spec
data Status
data ContentType 
data Method'
data Respond' (status :: Status) (headers :: Headers) 
type Headers = Row Type 
type OtherResponse = Row Type 


data Alt a b = Alt a b 
data Sub a b = Sub 


infixr 4 type Sub as :>
infixr 4 type Sub as :/

infixr 3 type Alt as :<|>
infixr 3 Alt as :<|>

foreign import data Seg        :: Symbol -> Spec 
foreign import data Capture    :: Type -> Spec 
foreign import data QueryParam :: Symbol -> Type -> Spec
foreign import data Header     :: Symbol -> Type -> Spec 
foreign import data Verb       :: Method' -> Type -> Status -> Headers -> ContentType -> Spec
foreign import data Respond    :: Status -> Headers -> ContentType -> Spec
foreign import data Raise      :: Status -> Headers -> ContentType -> Spec
foreign import data ReqBody    :: Type -> ContentType -> Spec
foreign import data Raw        :: Spec