module Swerve.Server.Internal.Handler where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Reader (ReaderT(..))
import Effect.Aff (Aff)
import Swerve.Server.Internal.ServerError (ServerError)

newtype Handler' spec conn a = Handler (ReaderT conn (ExceptT ServerError Aff) a)

type Handler spec a = forall conn. Handler' spec conn a 

derive newtype instance functorHandler :: Functor (Handler' spec conn)
derive newtype instance applyHandler :: Apply (Handler' spec conn)
derive newtype instance applicativeHandler :: Applicative (Handler' spec conn)
-- derive newtype instance bindHandler :: Bind (Handler route)
-- derive newtype instance monadHandler :: Monad (Handler route)

-- runHandler :: forall route a. Handler route a -> Effect (Either ServerError a)
-- runHandler (Handler handler) = runExceptT handler