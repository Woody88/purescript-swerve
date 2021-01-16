module Swerve.Client.Internal where

import Prelude 

import Network.Wai (defaultRequest)
import Swerve.Client.ClientM (ClientM')
import Swerve.Client.Internal.Eval (class EvalHandler, Client)
import Swerve.Client.Internal.HasClient (class HasClient, clientWithRoute)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

client :: forall api. 
  HasClient ClientM' api  
  => Proxy api 
  -> Client ClientM' api 
client api = clientWithRoute (Proxy :: _ ClientM') api defaultRequest 

lift :: forall a api. EvalHandler api a => Client ClientM' api -> a
lift = unsafeCoerce 