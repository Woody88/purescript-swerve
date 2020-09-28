module Swerve.Client.Internal where

import Network.Wai (defaultRequest)
import Swerve.Client.ClientM (ClientM)
import Swerve.Client.Internal.HasClient (class HasClient, clientWithRoute)
import Type.Proxy (Proxy(..))

client :: forall api conn result. 
  HasClient api conn ClientM result   
  => Proxy api -> Record conn -> result
client api conn = clientWithRoute api (Proxy :: _ ClientM) conn defaultRequest 