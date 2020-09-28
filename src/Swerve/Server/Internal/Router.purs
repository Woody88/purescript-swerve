module Swerve.Internal.Router where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

newtype Router a = Router (ExceptT String Aff a) 

derive newtype instance functorRouter :: Functor Router
derive newtype instance applyRouter :: Apply Router
derive newtype instance applicativeRouter :: Applicative Router
derive newtype instance bindRouter :: Bind Router
derive newtype instance monadRouter :: Monad Router
derive newtype instance monadEffectRouter :: MonadEffect Router
derive newtype instance monadAffRouter :: MonadAff Router
derive newtype instance monadThrowRouter :: MonadThrow String Router
derive newtype instance monadErrorRouter :: MonadError String Router

runRouter :: forall a. Router a -> Aff (Either String a)
runRouter (Router rt) = runExceptT rt 