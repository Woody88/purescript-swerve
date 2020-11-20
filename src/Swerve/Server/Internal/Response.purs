module Swerve.Server.Internal.Response where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, prj)
import Network.HTTP.Types (internalServerError500)
import Network.HTTP.Types as H
import Network.Wai (responseStr)
import Network.Wai as Wai
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Simple.JSON (writeJSON)
import Simple.JSON as JSON
import Swerve.API.Status (class HasStatus, BadRequest', Ok', getStatus)
import Swerve.API.Types (ContentType, Headers, Respond, Status)
import Type.Proxy (Proxy(..))

newtype Response (row :: Row Type) a = Response (Either RespondData RespondData)

-- derive newtype instance applyResponse :: Apply (Response row)
-- derive newtype instance applicativeResponse :: Applicative (Response row)

type RespondData 
  = { content :: String 
    , headers :: H.ResponseHeaders
    , status  :: H.Status
    }

-- class VariantResponse row (rl :: RowList Type) where 
--   variantResponse :: Proxy rl -> Variant row -> Wai.Response


-- instance variantResponseNil :: VariantResponse row RL.Nil where
--   variantResponse _ _ = responseStr internalServerError500 [] ""

-- instance variantResponseCons :: 
--   ( IsSymbol label
--   , WriteForeign content 
--   , VariantResponse row tail
--   , Row.Cons label (Respond content headers ctypes) r row
--   ) => VariantResponse row (RL.Cons label (Respond content headers ctypes) tail) where 
--   variantResponse _ resp = case prj label resp of 
--     Nothing          -> variantResponse tail resp 
--     Just (Respond r) -> responseStr r.status [] (writeJSON r.content)  
--     where 
--       label = SProxy :: _ label 
--       tail = Proxy :: _ tail 
type User = String 

_Ok = Proxy :: _ Ok'
_BadRequest = Proxy :: _ BadRequest'
-- z :: Response _ User 
-- z = case 13 of 
--   13        -> respond _Ok "User1"
--   otherwise -> raise BadRequest 

raise :: forall a r row status ctype label. 
  HasStatus status label 
  => Row.Cons label (Respond status () ctype) r row
  => Proxy status -> Response row a 
raise st = Response $ 
  Left { content: ""
       , status: getStatus (Proxy :: _ status)
       , headers: []
       }

respond :: forall a r row status ctype label. 
  JSON.WriteForeign a
  => HasStatus status label 
  => Row.Cons label (Respond status () ctype) r row
  => Proxy status -> a -> Response row a 
respond st a = Response $ 
  Right { content: writeJSON a 
        , status: getStatus (Proxy :: _ status)
        , headers: []
        }