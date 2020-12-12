module Swerve.Server.Internal.Response where

import Prelude

import Data.Either (Either(..))
import Network.HTTP.Types as H
import Swerve.API.Status (class HasStatus, getStatus)
import Swerve.API.Types (Respond', Status)
import Swerve.Server.Internal.ServerError (ServerError)
import Type.Proxy (Proxy(..))

class HasResp (status :: Status) (hdrs :: Row Type) (rs :: Type) 

instance hasRespReflexive :: HasResp a hdrs (Respond' a hdrs)
else instance hasRespLeft :: HasResp a hdrs (Either (Respond' a hdrs) b)
else instance hasRespRight :: HasResp a hdrs b => HasResp a hdrs (Either c b)

newtype Response :: forall k. k -> Type -> Type
newtype Response rs (a :: Type) = Response (Either ServerError (RespondData a))

unResponse :: forall rs a. Response rs a -> (Either ServerError (RespondData a))
unResponse (Response resp) = resp 

type RespondData a 
  = { content :: a 
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

raise' :: forall a rs status. 
  HasResp status ()  rs 
  => HasStatus status 
  => Proxy status -> String -> Response rs a 
raise' st s = Response $ 
    Left { content: s
         , status: getStatus st
         , headers: []
         }

respond :: forall a rs status. 
  HasResp status () rs
  => HasStatus status  
  => Proxy status -> a -> Response rs a 
respond st a = Response $ 
  Right { content: a 
        , status: getStatus (Proxy :: _ status)
        , headers: []
        }