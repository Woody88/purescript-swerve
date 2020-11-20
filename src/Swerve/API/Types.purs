module Swerve.API.Types where 

import Network.HTTP.Types as H

data Spec
data Status
data ContentType 
data Method
data Respond (status :: Status) (headers :: Headers) (ctypes :: ContentType)
type Headers = Row Type 
type OtherResponse = Row Type 


data Alt' a b = Alt' a b 

foreign import data Sub :: forall k. k -> Spec -> Spec 
foreign import data Alt :: Spec -> Spec -> Spec 

infixr 4 type Sub as :>
infixr 4 type Sub as :/

infixr 3 type Alt as :<|>
infixr 3 Alt' as :<|>

foreign import data Seg :: Symbol -> Spec 
foreign import data Capture :: Type -> Spec 
foreign import data QueryParam :: Symbol -> Type -> Spec
foreign import data Header :: Symbol -> Type -> Spec 
foreign import data Verb  :: Method -> Type -> Type -> Spec
foreign import data ReqBody :: Type -> ContentType -> Spec
foreign import data Raw     :: Spec 
