module Swerve.Server.Internal.Response where

import Prelude

import Data.Either (Either(..))
import Network.HTTP.Types as H
import Simple.JSON (writeJSON)
import Simple.JSON as JSON
import Swerve.API.Status (class HasStatus, getStatus)
import Swerve.API.Types (Respond', Status)
import Type.Proxy (Proxy(..))

class HasResp (status :: Status) (hdrs :: Row Type) (rs :: Type) 

instance hasRespReflexive :: HasResp a hdrs (Respond' a hdrs)
else instance hasRespLeft :: HasResp a hdrs (Either (Respond' a hdrs) b)
else instance hasRespRight :: HasResp a hdrs b => HasResp a hdrs (Either c b)

newtype Response :: forall k. k -> Type -> Type
newtype Response rs (a :: Type) = Response (Either RespondData RespondData)

type RespondData 
  = { content :: String 
    , headers :: H.ResponseHeaders
    , status  :: H.Status
    }

raise :: forall a rs status. 
  HasResp status ()  rs 
  => HasStatus status 
  => Proxy status -> Response rs a 
raise st = Response $ 
    Left { content: "" 
         , status: getStatus st
         , headers: []
         }

respond :: forall a rs status. 
  HasResp status () rs
  => JSON.WriteForeign a
  => HasStatus status  
  => Proxy status -> a -> Response rs a 
respond st a = Response $ 
  Right { content: writeJSON a 
        , status: getStatus (Proxy :: _ status)
        , headers: []
        }