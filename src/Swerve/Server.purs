module Swerve.Server 
  ( module Auth
  , module BasicAuth 
  , module Handler
  , module ServerError
  , module ServerType
  , module Status
  , module Response
  , module RowApply
  , serve
  , serveWithContext 
  , hoistServer
  , lift
  , compose
  ) where

import Prelude 

import Effect.Aff (Aff)
import Network.Wai (Application) 
import Swerve.Server.Internal.Eval (class EvalCompose, class EvalHandler, Regular, Compose, Server, ServerT)
import Swerve.Server.Internal.Eval (Server, ServerT, eval) as ServerType
import Swerve.Server.Internal (class HasServer, hoistServerWithContext, route)
import Swerve.Server.Internal.Auth (AuthHandler(..), mkAuthHandler, unAuthHandler) as Auth
import Swerve.Server.Internal.BasicAuth (BasicAuthCheck(..), BasicAuthResult(..)) as BasicAuth
import Swerve.Server.Internal.Delayed (emptyDelayed)
import Swerve.Server.Internal.Handler (HandlerM, Handler, runHandler) as Handler
import Swerve.Server.Internal.Response (ResponseV, Nil, respond, respond') as Response
import Swerve.Server.Internal.RoutingApplication (toApplication)
import Swerve.Server.Internal.Router (runRouter)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Status (BadRequest, Created, Forbidden, MovedPermanently, NoContent, NotFound, NotImplemented, Ok, Unauthorized, WithStatusString) as Status
import Swerve.Server.Internal.ServerError (ServerError, err400, err401, err403, err404, err405, err406, err415, err500, responseServerError) as ServerError
import Type.Proxy (Proxy(..))
import Type.Row (type (+)) as RowApply
import Unsafe.Coerce (unsafeCoerce)

serve :: forall api. 
  HasServer api () Aff
  => Proxy api -> Server api -> Application
serve p s = toApplication (runRouter (const ServerError.err404) (route p (Proxy :: _ Aff) {} (emptyDelayed (Route s))))

serveWithContext :: forall api context. 
  HasServer api context Aff
  => Proxy api -> Record context -> Server api -> Application
serveWithContext p ctx s = toApplication (runRouter (const ServerError.err404) (route p (Proxy :: _ Aff) ctx (emptyDelayed (Route s))))

hoistServer :: forall api ctx m n. 
  HasServer api ctx m 
  => Proxy api
  -> (forall x. m x -> n x) 
  -> ServerT api m 
  -> ServerT api n
hoistServer p = hoistServerWithContext p (Proxy :: Proxy ctx)

lift :: forall a api m. Monad m => EvalHandler Regular api a => a -> ServerT api m
lift = unsafeCoerce 

compose :: forall a api m. Monad m => EvalHandler Compose api a => a -> ServerT api m
compose = unsafeCoerce 