module Swerve.API.Types where 

data Status
data ContentType 
data Method'
data Respond' (status :: Status) (headers :: Headers) 

data Alt a b = Alt a b
data Sub a b

type Headers = Row Type 
type OtherResponse = Row Type 

infixr 4 type Sub as :>
infixr 4 type Sub as :/

infixr 3 type Alt as :<|>
infixr 3 Alt as :<|>

data Seg (s :: Symbol) 
data Capture (t :: Type) 
data QueryParam (s :: Symbol) (t :: Type)
data Header (s :: Symbol) (t :: Type)
data Verb (m :: Method') (a :: Type) (st :: Status) (hdrs :: Headers) (ct :: ContentType)
data Respond (st :: Status) (hdrs :: Headers) (a :: Type) 
data Raise (st :: Status) (hdrs :: Headers) (ct :: ContentType) 
data ReqBody (a :: Type) (ct :: ContentType) 
data Raw

data SVerb :: forall k. Method' -> ContentType -> k -> Type
data SVerb (m :: Method') (cts :: ContentType) as 