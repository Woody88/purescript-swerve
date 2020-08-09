module Swerve.Server where

import Prelude

import Data.Either (Either)
import Data.Symbol (SProxy(..))
import Swerve.Server.Internal (parsePath, parseRoute)
import Swerve.Server.Internal.Path (CaptureVar, PCons, PNil, PProxy(..))
import Type.Data.RowList (RLProxy(..))
import Type.Row (RProxy(..))

-- import Network.Wai (Application)
-- import Prim.RowList as RL
-- import Swerve.Server.Internal (Server, class RoutesHandlers, matchRoutesImpl)
-- import Swerve.Server.Internal.RouterApplication (toApplication)
-- import Type.Data.Row (RProxy(..))
-- import Type.Data.RowList (RLProxy(..))

-- swerve :: forall api apiL handlers
--    . RL.RowToList api apiL
--   => RoutesHandlers apiL api handlers
--   => Server api
--   -> Record handlers
--   -> Application
-- swerve api handlers = toApplication $ matchRoutesImpl (RLProxy :: RLProxy apiL) (RProxy :: RProxy api) handlers


urlSpec :: SProxy "/:id"
urlSpec = SProxy

specs :: RProxy (capture :: { id :: Int })
specs = RProxy

url :: String
url = "/user/1"

foo :: Either String
  { capture :: Record ()
  , id :: Int
  }
foo = parseRoute urlSpec specs url
