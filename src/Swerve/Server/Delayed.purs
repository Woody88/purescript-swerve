module Server.Delayed where

import Prelude

import Control.Monad.Reader (ask)
import Data.Function (applyFlipped)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Network.Wai (Request, Response)
import Swerve.Server.DelayedIO (DelayedIO, liftRouteResult, runDelayedIO)
import Swerve.Server.Response (Response) as Resp
import Swerve.Server.RouteResult (RouteResult(..))
import Unsafe.Coerce (unsafeCoerce)

data Delayed :: forall k. k -> Type -> Type
data Delayed env c 


type DelayedSpec env captures auth contentType params headers body c 
  = { captures :: env -> DelayedIO captures
    , method :: DelayedIO Unit
    , auth :: DelayedIO auth
    , accept :: DelayedIO Unit
    , content :: DelayedIO contentType
    , params :: DelayedIO params
    , headers :: DelayedIO headers
    , body :: contentType -> DelayedIO body
    , server :: captures -> params -> headers -> auth -> body -> Request -> RouteResult c
    }

instance functorDelayed :: Functor (Delayed env) where 
  map f = unDelayed \d@{ server } ->
    mkDelayed d { server = \c p h a b req -> f <$> server c p h a b req } 

mkDelayed ::
  forall env captures auth contentType params headers body c.
  DelayedSpec env captures auth contentType params headers body c ->
  Delayed env c
mkDelayed = unsafeCoerce

unDelayed ::
  forall env c r.
  ( forall captures auth contentType params headers body.
    DelayedSpec env captures auth contentType params headers body c -> 
    r
  ) ->
  Delayed env c ->
  r
unDelayed f = f <<< unsafeCoerce

-- | A 'Delayed' without any stored checks.
emptyDelayed :: forall env a. RouteResult a -> Delayed env a
emptyDelayed result =
  mkDelayed 
    { captures: (const r)
    , method: r 
    , auth: r 
    , accept: r 
    , content: r  
    , params: r 
    , headers: r 
    , body: (const r) 
    , server: (\ _ _ _ _ _ _ -> result)
    }
  where
    r = pure unit

addCapture :: forall a b captured env. Delayed env (a -> b) -> (captured -> DelayedIO a) -> Delayed (captured /\ env) b
addCapture delayed new = 
  delayed 
    # unDelayed \d@{ captures, server} -> 
        mkDelayed d { captures = \(txt /\ env) -> (/\) <$> captures env <*> new txt
                    , server   = \(Tuple x v) p h a b req -> (applyFlipped v) <$> (server x p h a b req)
                    }

addAuthCheck :: forall env a b. 
  Delayed env (a -> b)
  -> DelayedIO a
  -> Delayed env b
addAuthCheck delayed new =
  delayed 
    # unDelayed \d@{ auth, server} -> 
        mkDelayed d { auth  = Tuple <$> auth <*> new
                    , server   = \ c p h (Tuple y v) b req -> (applyFlipped v) <$> server c p h y b req
                    }

runDelayed :: forall a env.
  Delayed env a
  -> env
  -> Request
  -> Aff (RouteResult a)
runDelayed d env = 
    d 
    # unDelayed \delayed ->
        runDelayedIO $ do
          liftAff $Console.log "in"
          r <- ask
          c <- delayed.captures env
          delayed.method
          a <- delayed.auth
          delayed.accept
          content <- delayed.content
          p <- delayed.params       -- Has to be before body parsing, but after content-type checks
          h <- delayed.headers
          b <- delayed.body content
          liftRouteResult (delayed.server c p h a b r)

runAction :: forall env a r row. 
  Delayed env (Aff (Resp.Response row a))
  -> env
  -> Request
  -> (RouteResult Response -> Aff r)
  -> ((Resp.Response row a) -> RouteResult Response)
  -> Aff r
runAction action env req respond k = 
    runDelayed action env req >>= go >>= respond
  where
    go (Fail e)      = pure $ Fail e
    go (FailFatal e) = pure $ FailFatal e
    go (Route a)     = do
      x  <- a
      pure $ k x