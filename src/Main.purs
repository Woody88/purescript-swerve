module Main where

import Prelude

import Effect (Effect)
-- import Effect.Console (log)
-- import Network.Wai (Application)
-- import Network.Warp as Warp
-- import Swerve.API.Verb (Get)
-- import Swerve.Server.Internal (swerve)
-- import Swerve.Server.Internal.Handler (Handler(..))
-- import Type.Proxy (Proxy(..))

-- newtype User = User String 

-- type MyAPI = GetUser

-- type GetUser 
--   = Get "/user" 
--       (content :: User)

-- handler :: Handler GetUser User
-- handler = Handler $ pure $ User "hello"

-- app :: Application 
-- app = swerve (Proxy :: _ MyAPI) handler  

main :: Effect Unit
main = pure unit -- Warp.run 8000 app
