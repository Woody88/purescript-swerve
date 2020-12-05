module Swerve.Server.Internal.ServerContext where

import Effect.Aff 

class ServerContext m n | m -> n, n -> m

data Server' (api :: Type)  (m :: Type -> Type) 

type Server spec = Server' spec Aff

instance serverContextServer :: ServerContext (Server' api m) n 