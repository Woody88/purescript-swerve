module Swerve.Server.Internal where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Wai (Application, Request(..), responseStr)
import Network.Wai as Wai
import Prim.RowList (class RowToList)
import Simple.JSON (class WriteForeign, writeJSON)
import Swerve.API.Capture (class ReadCapture, readCapture)
import Swerve.API.QueryParam (class ReadQuery, readQuery)
import Swerve.API.Status (class HasStatus, getStatus)
import Swerve.API.Types (type (:>), Capture, QueryParam, Spec, Verb)
import Swerve.Server.Internal.Delayed (Delayed, addCapture, addParameterCheck, emptyDelayed, runAction)
import Swerve.Server.Internal.DelayedIO (delayedFail, withRequest)
import Swerve.Server.Internal.Response (class VariantResponse, Response(..), variantResponse)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.Router (Router, Router'(..), leafRouter, pathRouter, runRouter)
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Swerve.Server.Internal.ServerError (err400, err404)
import Type.Equality (class TypeEquals)
import Type.Equality as TypeEquals
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Server' ( spec :: Spec) (m :: Type -> Type)

type Server spec = Server' spec Aff
 
class EvalServer server handler | server -> handler

instance evalServerVerb :: EvalServer (Server' (Verb status a hdrs ctypes row) Aff) (Aff (Response row a))
instance evalServerCapture  ::  EvalServer (Server' (Capture a :> api) Aff) (a -> Server' api Aff)
instance evalServerQueryParam  ::  IsSymbol sym => EvalServer (Server' (QueryParam sym a :> api) Aff) (Maybe a -> Server' api Aff)
instance evalServerSeg  :: IsSymbol path => EvalServer (Server' (path :> api) Aff) (Server' api Aff)

class HasServer api context handler | api -> context handler where 
  route :: forall env. Proxy api -> Record context -> Delayed env (Server api) -> Router env

instance hasServerVerb :: 
  ( RowToList row rl
  , HasStatus status label 
  , WriteForeign a 
  , VariantResponse row rl 
  ) => HasServer (Verb status a hdrs ctypes row) context (Aff (Response row a)) where 
  route _ _ subserver = leafRouter route'
    where 
      status = Proxy :: _ status 
      route' env request respond = runAction (toHandler subserver) env request respond 
        \(Response v) -> case v of 
            Right a -> Route $ responseStr (getStatus status) [] (writeJSON a)
            Left vr -> Route $ variantResponse (Proxy :: _ rl) vr

instance hasServerCapture :: 
  ( ReadCapture a
  , HasServer api context handler
  ) => HasServer (Capture a :> api) context (a -> handler) where 
  route _ ctx subserver =
    CaptureRouter $ 
      route (Proxy :: _ api) ctx
        (addCapture (toHandler subserver) \c -> withRequest \request -> 
          case readCapture c of 
            Nothing           -> delayedFail err400 
            Just (piece :: a) -> pure piece)

instance hasServerQuery :: 
  ( IsSymbol sym
  , ReadQuery a
  , HasServer api context handler
  ) => HasServer (QueryParam sym a :> api) context (Maybe a -> handler) where 
  route _ ctx subserver = route (Proxy :: _ api) ctx delayed
  
    where 
      delayed = addParameterCheck (toHandler subserver) <<< withRequest $ 
        \(Request req) -> let 
          key = reflectSymbol (SProxy :: _ sym)
          qMap = Map.fromFoldable $ req.queryString
          in case Map.lookup key qMap of 
            Nothing -> delayedFail err400 { content = req.url }
            Just (piece :: Maybe String)   -> pure $ piece >>= readQuery

instance hasServerSegment :: 
  ( IsSymbol path
  , HasServer api context handler
  ) => HasServer (path :> api) context handler where 
  route _ ctx subserver = pathRouter path (route (Proxy :: _ api) ctx (toHandler subserver))
    where 
      path = reflectSymbol (SProxy :: _ path)

from :: forall handlers api context. HasServer api context handlers => handlers -> Server api
from = unsafeCoerce

toHandler :: forall server handler env. EvalServer server handler => Delayed env server -> Delayed env handler
toHandler = unsafeCoerce

serve :: forall api handler. 
  HasServer api () handler 
  => Proxy api -> Server api -> Application
serve p serv = toApplication (runRouter (const err404) (route p {} (emptyDelayed (Route serv))))

serve' :: forall api handler context. 
  HasServer api context handler 
  => Proxy api -> Record context -> Server api -> Application
serve' p ctx serv = toApplication (runRouter (const err404) (route p ctx (emptyDelayed (Route serv))))