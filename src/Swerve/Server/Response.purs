module Swerve.Server.Response where

import Prelude

import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Variant (Variant)
import Network.HTTP.Types as H
import Swerve.API.Types (ContentType, Headers)

newtype Response (row :: Row Type) a = Response (Either (Variant row) a)

derive newtype instance functorResponse :: Functor (Response row)
derive newtype instance applyResponse :: Apply (Response row)
derive newtype instance applicativeResponse :: Applicative (Response row)

newtype Respond (content :: Type) (headers :: Headers) (ctypes :: ContentType)
  = Respond { content :: content 
            , headers :: Record headers
            , status  :: H.Status
            }

derive instance newtypeRespond :: Newtype (Respond c h ct) _