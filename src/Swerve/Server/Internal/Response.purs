module Swerve.Server.Internal.Response where

import Prelude

import Data.Either (Either)
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
import Simple.JSON (class WriteForeign, writeJSON)
import Swerve.API.Types (ContentType, Headers)
import Type.Proxy (Proxy(..))

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

class VariantResponse row (rl :: RowList Type) where 
  variantResponse :: Proxy rl -> Variant row -> Wai.Response


instance variantResponseNil :: VariantResponse row RL.Nil where
  variantResponse _ _ = responseStr internalServerError500 [] ""

instance variantResponseCons :: 
  ( IsSymbol label
  , WriteForeign content 
  , VariantResponse row tail
  , Row.Cons label (Respond content headers ctypes) r row
  ) => VariantResponse row (RL.Cons label (Respond content headers ctypes) tail) where 
  variantResponse _ resp = case prj label resp of 
    Nothing          -> variantResponse tail resp 
    Just (Respond r) -> responseStr r.status [] (writeJSON r.content)  
    where 
      label = SProxy :: _ label 
      tail = Proxy :: _ tail 