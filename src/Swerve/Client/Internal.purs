module Swerve.Client.Internal where

import Network.Wai (defaultRequest)
import Swerve.Client.ClientM (ClientM)
import Swerve.Client.Internal.HasClient (class HasClient, clientWithRoute)
import Type.Proxy (Proxy)

client :: forall a api. HasClient ClientM api a => Proxy api -> ClientM a
client api = clientWithRoute api defaultRequest