module Swerve.Client.Internal where

import Network.Wai (defaultRequest)
import Swerve.Client.ClientM (ClientM)
import Swerve.Client.Internal.HasClient (class HasClient, clientWithRoute)
import Type.Proxy (Proxy)

client :: forall a api conn. 
  HasClient api conn ClientM a 
  => Proxy api -> Record conn -> ClientM a
client api conn = clientWithRoute api conn defaultRequest 