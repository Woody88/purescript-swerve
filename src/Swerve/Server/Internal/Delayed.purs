module Swerve.Server.Internal.Delayed where

import Prelude

import Control.Monad.Reader (ask)
import Data.Function (applyFlipped)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff (Aff)
import Network.Wai (Request, Response)
import Prim.Row as Row 
import Record as Record 
import Record.Unsafe (unsafeGet) as Record
import Swerve.Server.Internal.DelayedIO (DelayedIO, liftRouteResult, runDelayedIO)
import Swerve.Server.Internal.Handler
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Type.Proxy (Proxy)
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

modifyServer :: forall a b r name env. IsSymbol name => Delayed env { | a } -> Proxy name -> Delayed env b
modifyServer delayed _ = do 
  let n = reflectSymbol (SProxy :: _ name)
  let delayed' = Record.unsafeGet n  <$> delayed 
  delayed'
    # unDelayed \d@{ server } -> 
        mkDelayed d { server = \c p h a b req -> server c p h a b req
                    }

addCapture :: forall a b captured env. Delayed env (a -> b) -> (captured -> DelayedIO a) -> Delayed (captured /\ env) b
addCapture delayed new = 
  delayed 
    # unDelayed \d@{ captures, server} -> 
        mkDelayed d { captures = \(txt /\ env) -> (/\) <$> captures env <*> new txt
                    , server   = \(Tuple x v) p h a b req -> (applyFlipped v) <$> (server x p h a b req)
                    }

addParameterCheck :: forall env a b. 
  Delayed env (a -> b)
  -> DelayedIO a
  -> Delayed env b
addParameterCheck delayed new =
  delayed 
    # unDelayed \d@{ params, server} -> 
        mkDelayed d { params  = Tuple <$> params <*> new
                    , server  = \ c (Tuple p pNew) h a b req -> (applyFlipped pNew) <$> server c p h a b req
                    }

addHeaderCheck :: forall env a b. 
  Delayed env (a -> b)
  -> DelayedIO a
  -> Delayed env b
addHeaderCheck delayed new =
  delayed 
    # unDelayed \d@{ headers, server} -> 
        mkDelayed d { headers  = Tuple <$> headers <*> new
                    , server  = \ c p (Tuple h hNew) a b req -> (applyFlipped hNew) <$> server c p h a b req
                    }

addMethodCheck :: forall env a. 
  Delayed env a
  -> DelayedIO Unit
  -> Delayed env a
addMethodCheck delayed new =
  delayed 
    # unDelayed \d@{ method } -> 
        mkDelayed d { method = method <* new } 

addAcceptCheck :: forall env a. 
  Delayed env a
  -> DelayedIO Unit
  -> Delayed env a
addAcceptCheck delayed new =
  delayed 
    # unDelayed \d@{ accept } -> 
        mkDelayed d { accept = accept <* new } 

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

addBodyCheck :: forall env a b ctype. 
  Delayed env (a -> b)
  -> DelayedIO ctype         -- ^ content type check
  -> (ctype -> DelayedIO a)  -- ^ body check
  -> Delayed env b
addBodyCheck delayed newContent newBody =
  delayed 
    # unDelayed \d@{ content, body, server} -> 
        mkDelayed d { content = Tuple <$> content <*> newContent 
                    , body    = \(Tuple ctype c) -> Tuple <$> body ctype <*> newBody c 
                    , server  = \c p h a (Tuple b newB) req -> (applyFlipped newB) <$> server c p h a b req 
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

runAction :: forall env a r. 
  Delayed env (HandlerM a)
  -> env
  -> Request
  -> (RouteResult Response -> Aff r)
  -> (a -> RouteResult Response)
  -> Aff r
runAction action env req respond k = 
    runDelayed action env req >>= go >>= respond
  where
    go (Fail e)      = pure $ Fail e
    go (FailFatal e) = pure $ FailFatal e
    go (Route a)     = do
      x  <- runHandler a
      pure $ k x