module Swerve.Client.Internal.Auth where 

import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Swerve.Client.Internal.Request 

newtype AuthenticatedRequest a = AuthenticatedRequest (Tuple a (a -> Request -> Request))

unAuthReq :: forall a. AuthenticatedRequest a -> (Tuple a (a -> Request -> Request))
unAuthReq (AuthenticatedRequest a) = a 

mkAuthenticatedRequest :: forall a. a -> (a -> Request -> Request) -> AuthenticatedRequest a
mkAuthenticatedRequest val func = AuthenticatedRequest (val /\ func)