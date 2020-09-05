module Swerve.Server.Internal.Handler where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Swerve.Server.Internal.ServerError (ServerError)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

data Params spec
newtype Handler specs a = Handler (ReaderT (Params specs) (ExceptT String Aff) a)

toParams :: forall spec params. Conn spec params => Proxy spec -> { | params } -> Params spec
toParams _ = unsafeCoerce

fromParams :: forall spec params. Conn spec params => Params spec -> { | params }
fromParams = unsafeCoerce

derive newtype instance functorHandler :: Functor (Handler spec)
derive newtype instance applyHandler :: Apply (Handler spec)
derive newtype instance applicativeHandler :: Applicative (Handler spec)
derive newtype instance bindHandler :: Bind (Handler spec)
derive newtype instance monadHandler :: Monad (Handler spec)
derive newtype instance monadEffect :: MonadEffect (Handler spec)
derive newtype instance monadAff :: MonadAff (Handler spec)
instance monadAskHandler :: Conn spec params => MonadAsk { | params } (Handler spec) where
  ask = Handler (fromParams <$> ask)