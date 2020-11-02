module Swerve.Server.Internal.Router where

import Prelude

import Data.Array as Array
import Data.Function (on)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Network.Wai (Response)
import Swerve.Server.Internal.ErrorFormatter (NotFoundErrorFormatter)
import Swerve.Server.Internal.RouteResult (RouteResult(..))
import Swerve.Server.Internal.RoutingApplication (RoutingApplication)

type Router env = Router' env RoutingApplication

data Router' env a =
    StaticRouter  (Map String (Router' env a)) (Array (env -> a))
      -- ^ the map contains routers for subpaths (first path component used
      --   for lookup and removed afterwards), the list contains handlers
      --   for the empty path, to be tried in order
  | CaptureRouter (Router' (String /\ env) a)
      -- ^ first path component is passed to the child router in its
      --   environment and removed afterwards
  | CaptureAllRouter (Router' (Array String /\ env) a)
      -- ^ all path components are passed to the child router in its
      --   environment and are removed afterwards
  | RawRouter     (env -> a)
      -- ^ to be used for routes we do not know anything about
  | Choice        (Router' env a) (Router' env a)
      -- ^ left-biased choice between two routers

derive instance functorRouter :: Functor (Router' env)

-- | Smart constructor for a single static path component.
pathRouter :: forall env a. String -> Router' env a -> Router' env a
pathRouter t r = StaticRouter (M.singleton t r) []

-- | Smart constructor for a leaf, i.e., a router that expects
-- the empty path.
leafRouter :: forall env a. (env -> a) -> Router' env a
leafRouter l = StaticRouter M.empty [l]

choice :: forall env a. Router' env a -> Router' env a -> Router' env a
choice (StaticRouter table1 ls1) (StaticRouter table2 ls2) =
  StaticRouter (M.unionWith choice table1 table2) (ls1 <> ls2)
choice (CaptureRouter router1)   (CaptureRouter router2)  =
  CaptureRouter (choice router1 router2)
choice router1 (Choice router2 router3) = Choice (choice router1 router2) router3
choice router1 router2 = Choice router1 router2

tweakResponse :: forall env. (RouteResult Response -> RouteResult Response) -> Router env -> Router env
tweakResponse f = map (\app req resp -> app req (resp <<< f))

runRouter :: NotFoundErrorFormatter -> Router Unit -> RoutingApplication
runRouter fmt r = runRouterEnv fmt r unit

runRouterEnv :: forall env. NotFoundErrorFormatter -> Router env -> env -> RoutingApplication
runRouterEnv fmt router env request respond  =
  case router of
    StaticRouter table ls ->
      case _.pathInfo $ unwrap request of
        []   -> runChoice fmt ls env request respond
        arr | Just {head: first, tail: rest}  <- Array.uncons arr 
            , Just router'                   <- M.lookup first table
            ->  let request' = wrap $  _ { pathInfo = rest } $ unwrap request 
                in  runRouterEnv fmt router' env request' respond
        _ -> respond $ Fail $ fmt request
    CaptureRouter router' ->
      case _.pathInfo $ unwrap request of
        arr | Just {head: first, tail: rest}  <- Array.uncons arr 
            ->  let request' = wrap $  _ { pathInfo = rest } $ unwrap request 
                in  runRouterEnv fmt router' (first /\ env) request' respond
        otherwise -> respond $ Fail $ fmt request
    CaptureAllRouter router' ->
      let segments = _.pathInfo $ unwrap request
          request' = wrap $  _ { pathInfo = [] } $ unwrap request 
      in runRouterEnv fmt router' (segments /\ env) request' respond
    RawRouter app ->
      app env request respond
    Choice r1 r2 ->
      runChoice fmt [runRouterEnv fmt r1, runRouterEnv fmt r2] env request respond

runChoice :: forall env. NotFoundErrorFormatter -> Array (env -> RoutingApplication) -> env -> RoutingApplication
runChoice fmt ls =
  case ls of
    [r]      -> r
    fs | Just {head: r, tail: rs } <- Array.uncons fs ->
            \ env request respond ->
            r env request $ \ response1 ->
            case response1 of
                Fail _ -> runChoice fmt rs env request $ \ response2 ->
                    respond $ highestPri response1 response2
                _      -> respond response1
        | otherwise -> \ _ request respond -> respond (Fail $ fmt request)
  where
    highestPri (Fail e1) (Fail e2) =
      if worseHTTPCode (e1.status.code) (e2.status.code)
        then Fail e2
        else Fail e1
    highestPri (Fail _) y = y
    highestPri x _ = x

-- Priority on HTTP codes.
worseHTTPCode :: Int -> Int -> Boolean
worseHTTPCode = on (<) toPriority
  where
    toPriority :: Int -> Int
    toPriority 404 = 0 -- not found
    toPriority 405 = 1 -- method not allowed
    toPriority 401 = 2 -- unauthorized
    toPriority 415 = 3 -- unsupported media type
    toPriority 406 = 4 -- not acceptable
    toPriority 400 = 6 -- bad request
    toPriority _   = 5