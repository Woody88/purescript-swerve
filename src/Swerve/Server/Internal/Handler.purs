module Swerve.Server.Internal.Handler where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError, class MonadTrans, ExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Swerve.Server.Internal.ServerError (ServerError)
import Swerver.Server.Internal.Conn (class Conn)
import Unsafe.Coerce (unsafeCoerce)

data Params spec
newtype HandlerT specs m a = HandlerT (ReaderT (Params specs) m a)

type Handler specs a = HandlerT specs (ExceptT ServerError Aff) a 

toParams :: forall spec params proxy . Conn spec params => proxy spec -> { | params } -> Params spec
toParams _ = unsafeCoerce

fromParams :: forall spec params. Conn spec params => Params spec -> { | params }
fromParams = unsafeCoerce

derive newtype instance functorHandler :: Functor m => Functor (HandlerT spec m) 
derive newtype instance applyHandler :: Monad m => Apply (HandlerT spec m)
derive newtype instance applicativeHandler :: Monad m => Applicative (HandlerT spec m)
derive newtype instance bindHandler :: Monad m => Bind (HandlerT spec m)
derive newtype instance monadHandler :: Monad m => Monad (HandlerT spec m)
derive newtype instance monadEffectHandler :: MonadEffect m => MonadEffect (HandlerT spec m)
derive newtype instance monadAffHandler :: MonadAff m => MonadAff (HandlerT spec m)
derive newtype instance monadTransHandler :: MonadTrans (HandlerT spec)
derive newtype instance monadThrowHandler :: MonadThrow ServerError m => MonadThrow ServerError (HandlerT spec m)
derive newtype instance monadErrorHandler :: MonadError ServerError m => MonadError ServerError (HandlerT spec m)

instance monadAskHandler :: (Monad m, Conn spec params) => MonadAsk { | params } (HandlerT spec m) where
  ask = HandlerT (fromParams <$> ask)