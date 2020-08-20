module Swerve.Server.Internal.Handler where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT(..), ask, runReaderT)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)
import Prim.Row as Row
import Swerve.API.Verb (Verb)
import Swerve.Server.Internal.ServerError (ServerError)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Params spec
newtype Handler specs a = Handler (ReaderT (Params specs) (ExceptT String Effect) a)

toParams :: forall spec params. Conn spec params => Proxy spec -> { | params } -> Params spec
toParams _ = unsafeCoerce

fromParams :: forall spec params. Conn spec params => Params spec -> { | params }
fromParams = unsafeCoerce

-- newtype Handler specs a = Handler (forall conn. Conn specs conn => (ReaderT { | conn } (ExceptT String Effect) a))

-- type Handle specs a = Handler specs a 

derive newtype instance functorHandler :: Functor (Handler spec)
derive newtype instance applyHandler :: Apply (Handler spec)
derive newtype instance applicativeHandler :: Applicative (Handler spec)
derive newtype instance bindHandler :: Bind (Handler spec)
derive newtype instance monadHandler :: Monad (Handler spec)
instance monadAskHandler :: Conn spec params => MonadAsk { | params } (Handler spec) where
  ask = Handler (fromParams <$> ask)

-- runHandler :: forall specs a conn specs m p t. 
--   Row.Union conn t specs
--   => { | conn }
--   -> Handle (Verb m p specs) { | conn } a 
--   -> Effect (Either String a)
-- runHandler conn (Handler handler) = runExceptT $ runReaderT handler conn

-- asks' ::  forall specs a conn specs v m p t.
--   Row.Union conn t specs
--   => Handle (Verb m p specs) { | conn} a  
--   -> ({ | conn } -> v) 
--   -> Handle (Verb m p specs) { | conn} v 
-- asks' = ?hole