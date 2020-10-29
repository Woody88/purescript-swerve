module Swerve.Server.DelayedIO where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, lift, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Network.Wai (Request)
import Swerve.Server.RouteResult (RouteResult(..), RouteResultT(..), runRouteResultT)
import Swerve.Server.ServerError (ServerError)

newtype DelayedIO a = DelayedIO (ReaderT Request (RouteResultT Aff) a)

derive newtype instance functorDelayedIO :: Functor DelayedIO  
derive newtype instance appplyDelayedIO :: Apply DelayedIO
derive newtype instance applicativeDelayedIO :: Applicative DelayedIO
derive newtype instance bindDelayedIO :: Bind DelayedIO
derive newtype instance monadDelayedIO :: Monad DelayedIO
derive newtype instance monadAskDelayedIO :: MonadAsk Request DelayedIO
derive newtype instance monadReaderDelayedIO :: MonadReader Request DelayedIO
derive newtype instance monadEffectDelayedIO :: MonadEffect DelayedIO
derive newtype instance monadAffDelayedIO :: MonadAff DelayedIO

liftRouteResult :: forall a. RouteResult a -> DelayedIO a
liftRouteResult x = DelayedIO $ lift $ RouteResultT <<< pure $ x

runDelayedIO :: forall a. DelayedIO a -> Request -> Aff (RouteResult a)
runDelayedIO (DelayedIO m) req = runRouteResultT $ runReaderT m req

delayedFail :: forall a. ServerError -> DelayedIO a
delayedFail err = liftRouteResult $ Fail err

delayedFailFatal :: forall a. ServerError -> DelayedIO a
delayedFailFatal err = liftRouteResult $ FailFatal err

withRequest :: forall a. (Request -> DelayedIO a) -> DelayedIO a
withRequest f = do
    req <- ask
    f req