module Swerve.Server.Internal.Handler where 

import Prelude 

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Swerve.Server.Internal.Response (ResponseV)

type Handler a = HandlerM (ResponseV a)
newtype HandlerM a = Handler (Aff a)      

derive instance functorHandler :: Functor HandlerM 
derive newtype instance applyHandler :: Apply HandlerM
derive newtype instance applicativeHandler :: Applicative HandlerM
derive newtype instance bindHandler :: Bind HandlerM
derive newtype instance monadHandler :: Monad HandlerM
derive newtype instance monadEffectHandler :: MonadEffect HandlerM
derive newtype instance monadAffHandler :: MonadAff HandlerM

runHandler :: forall a. HandlerM a -> Aff a
runHandler (Handler a) = a