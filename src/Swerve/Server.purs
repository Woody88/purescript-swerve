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


url :: String
url = "/user/1"

capAndQuery :: Either String
  { capture :: { id :: Int
               }
  , query :: { minAge :: Int
             }
  }
capAndQuery =
  parseRoute
  (SProxy :: _ "/:id?[minAge]")
  (RProxy :: _ (capture :: { id :: Int }, query :: { minAge :: Int }))
  url

capOnly :: Either String
  { capture :: { id :: Int
               }
  }
capOnly =
  parseRoute
  (SProxy :: _ "/:id")
  (RProxy :: _ (capture :: { id :: Int }))
  url
