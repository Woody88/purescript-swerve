module Swerve.Server.Internal.Response where

import Prelude

import Data.Either (Either(..), either)
import Data.Either.Inject (class Inject, inj)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy(..))
import Network.HTTP.Types (internalServerError500)
import Network.HTTP.Types as H
import Network.Wai (responseStr)
import Network.Wai as Wai
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Simple.JSON (writeJSON)
import Simple.JSON as JSON
import Swerve.API.Status (class HasStatus, _Ok, getStatus)
import Swerve.API.Types (ContentType, Headers, Respond, Respond', Status)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class HasResp status (hdrs :: Row Type) rs 

instance hasRespReflexive :: HasResp a hdrs (Respond' a hdrs)
else instance hasRespLeft :: HasResp a hdrs (Either (Respond' a hdrs) b)
else instance hasRespRight :: HasResp a hdrs b => HasResp a hdrs (Either c b)

newtype Response rs (a :: Type) = Response (Either RespondData RespondData)

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

-- z :: Response _ User 
-- z = case 13 of 
--   13        -> respond _Ok "User1"
--   otherwise -> raise BadRequest 

raise :: forall a rs status ctype. 
  HasResp status ()  rs 
  => HasStatus status 
  => Proxy status -> Response rs a 
raise st = Response $ 
    Left { content: "" 
         , status: getStatus st
         , headers: []
         }

respond :: forall a hdrs rs status ctype. 
  HasResp status () rs
  => JSON.WriteForeign a
  => HasStatus status  
  => Proxy status -> a -> Response rs a 
respond st a = Response $ 
  Right { content: writeJSON a 
        , status: getStatus (Proxy :: _ status)
        , headers: []
        }
