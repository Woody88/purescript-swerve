module Swerve.Client.Internal.RunClient where

import Prelude
import Data.Variant (Variant)
import Network.Wai (Request, Response)

class Monad m <= RunClient m where
  runRequest :: Request -> m Response
  throwClientError :: forall a. String -> m a