module Swerve.Server.Internal.Handler where

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad)

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either)
import Effect (Effect)
import Swerve.Server.Internal.ServerError (ServerError)

newtype Handler route a = Handler (ExceptT ServerError Effect a)

derive newtype instance functorHandler :: Functor (Handler route)
derive newtype instance applyHandler :: Apply (Handler route)
derive newtype instance applicativeHandler :: Applicative (Handler route)
derive newtype instance bindHandler :: Bind (Handler route)
derive newtype instance monadHandler :: Monad (Handler route)

runHandler :: forall route a. Handler route a -> Effect (Either ServerError a)
runHandler (Handler handler) = runExceptT handler