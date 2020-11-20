module Test.HList where

import Prelude

import Data.Either (Either)
import Network.HTTP.Types as H
import Type.Proxy (Proxy(..))

data HCons (a :: Status) b 

class Inject (a :: Status) b 

instance injectReflexive :: Inject a a

else instance injectLeft :: Inject a (HCons a b)

else instance injectRight :: Inject a b => Inject a (HCons c b)

data Response rs a = Response { content :: String }

data Status 

foreign import data Ok :: Status
foreign import data BadRequest :: Status
foreign import data NotFound :: Status


class HasStatus a where 
  getStatus :: Proxy a -> H.Status 

instance hasStatusOk :: HasStatus Ok where 
  getStatus _ = H.ok200

instance hasStatusBadRequest :: HasStatus BadRequest where 
  getStatus _ = H.badRequest400

instance hasStatusNotFound :: HasStatus NotFound where 
  getStatus _ = H.notFound404

raise :: forall status statuses a. 
  HasStatus status 
  => Inject status statuses 
  => Proxy status -> Response statuses a
raise p =  Response { content: _.message $ getStatus p }

resp :: forall status statuses a.  
  HasStatus status 
  => Inject status statuses
  => Proxy status -> a -> Response statuses a
resp p _ = Response { content: _.message $ getStatus p }   


-- z :: Response _ String 
-- z = resp (Proxy  :: _ Ok) "hello"

z :: forall statuses. 
  Inject BadRequest statuses
  => Inject Ok statuses
  => Response statuses String
z = case 13 of 
  13 -> raise (Proxy :: _ BadRequest)
  -- 14 -> raise (Proxy :: _ NotFound)
  otherwise -> resp (Proxy :: _ Ok) "hello"

-- resp _ _ = R {content: ""}

-- data HList 
 
-- foreign import data HNil :: HList 
-- foreign import data HCons :: Type -> HList -> HList 

-- infixr 4 type HCons as :

-- type H a = a : HNil
 
-- class HAppend head (tail :: HList) (list :: HList) | head tail -> list 

-- instance append :: HAppend h t (h : t)

-- class IsElem elem hlist (bool :: Boolean) | elem hlist -> bool

-- instance isElemTrue :: IsElem h (h : next) True
-- else instance isElemNil :: IsElem h (h' : HNil) False
-- else instance isElemLoop :: IsElem h a bool => IsElem h (h' : a) bool 

-- x :: forall a list. HAppend a HNil list => a -> Proxy list 
-- x _ = Proxy 

-- y :: forall a list bool. IsElem a list bool => a -> Proxy list -> Proxy bool 
-- y _ _ = Proxy 

-- data R :: forall k1 k2. Row k1 -> k2 -> Type
-- data R status a = R Resp

-- type Resp = { content :: String }

-- z :: R _ String 
-- z = case 13 of 
--   13 -> raise (Proxy :: _ BadRequest)
--   14 -> raise (Proxy :: _ NotFound)
--   otherwise -> resp (Proxy :: _ Ok) "hello"

-- raise :: forall row r status lbl a.   
--   HasStatus status lbl 
--   => Row.Cons lbl status r row 
--   => Proxy status -> R row a 
-- raise _ = R { content: "" }

-- resp :: forall row r status lbl a. 
--   HasStatus status lbl 
--   => Row.Cons lbl status r row
--   => Proxy status -> a -> R row a 
-- resp _ _ = R {content: ""}