module Swerve.Server.Internal.Auth where

import Data.Either (Either)
import Effect.Aff (Aff)
import Swerve.Server.Internal.ServerError (ServerError)

newtype AuthHandler r usr = AuthHandler (r -> Aff (Either ServerError usr))

mkAuthHandler :: forall r usr. (r -> Aff (Either ServerError usr)) -> AuthHandler r usr
mkAuthHandler = AuthHandler

unAuthHandler :: forall r usr. AuthHandler r usr -> (r -> Aff (Either ServerError usr)) 
unAuthHandler (AuthHandler f) = f 