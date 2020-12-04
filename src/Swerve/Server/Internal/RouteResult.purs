module Swerve.Server.Internal.RouteResult where 

import Prelude

import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Swerve.Server.ServerError (ServerError)

-- | The result of matching against a path in the route tree.
data RouteResult a 
  = Fail ServerError        -- ^ Keep trying other paths.
  | FailFatal ServerError   -- ^ Don't try other paths.
  | Route a

derive instance eqRouteResult :: Eq a => Eq (RouteResult a)
derive instance genericRouteResult :: Generic (RouteResult a) _
derive instance functorRouteResult :: Functor RouteResult 
instance applyRouteResult :: Apply RouteResult where
    apply = ap

instance applicativeRouteResult :: Applicative RouteResult where
    pure = Route

instance bindRouteResult :: Bind RouteResult where
    bind m f = case m of 
      Route a     -> f a
      Fail e      -> Fail e
      FailFatal e -> FailFatal e

instance monadRouteResult :: Monad RouteResult

instance debugRouteResult :: Debug a => Debug (RouteResult a) where
  debug = genericDebug

newtype RouteResultT m a = RouteResultT (m (RouteResult a))

derive instance functorRouteResultT :: Functor m => Functor (RouteResultT m)

instance monadTransRouteResultT :: MonadTrans RouteResultT where
    lift = RouteResultT <<< liftM1 Route

instance monadEffectRouteResult :: MonadEffect m => MonadEffect (RouteResultT m) where 
  liftEffect = lift <<< liftEffect

instance monadAffRouteResult :: MonadAff m => MonadAff (RouteResultT m) where
    liftAff = lift <<< liftAff

instance applyRouteResultT :: Monad m => Apply (RouteResultT m) where
    apply = ap

instance applicativeRouteResultT :: Monad m => Applicative (RouteResultT m) where
    pure = RouteResultT <<< pure <<< Route 

instance bindRouteResultT :: Monad m => Bind (RouteResultT m) where
    bind m f = RouteResultT do 
      a <- runRouteResultT m
      case a of 
        Route x     -> runRouteResultT (f x)
        Fail e      -> pure $ Fail e
        FailFatal e -> pure $ FailFatal e

instance monadRouteResultT :: Monad m => Monad (RouteResultT m) 

runRouteResultT :: forall m a. RouteResultT m a -> m (RouteResult a)
runRouteResultT (RouteResultT result) = result 