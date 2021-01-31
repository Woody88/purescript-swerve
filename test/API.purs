module Test.API where 

import Prelude 

import Data.Either
import Data.Maybe 
import Effect.Class
import Effect.Aff
import Network.Wai 
import Network.Warp.Settings
import Network.Warp.Run
import Node.HTTP as HTTP
import Node.Net.Server as Net
import Partial.Unsafe (unsafePartial)
import Swerve.Client.ClientM
import Swerve.API
import Swerve.Server  
import Swerve.Server as Server
import Type.Proxy 
import Unsafe.Coerce

type Person = 
  { name :: String
  , age  :: Int
  } 

type API = Record 
  ( person :: "person" :> PersonAPI
  )

type PersonAPI = Record 
  ( jim :: Get JSON (Ok Person + Nil)
  )

jimmy :: Person 
jimmy = { name: "Jimmy", age: 30 }

person :: Server PersonAPI 
person = Server.lift { jim } 
  where 
    jim :: Handler _
    jim = pure $ respond (Proxy :: _ Ok') jimmy

server :: Server API
server = Server.compose { person }

apiApp :: Application
apiApp = serve (Proxy :: _ API) server

serveStubbedApi :: Settings -> Application -> Aff HTTP.Server
serveStubbedApi settings app = liftEffect $ runSettings settings app 

stopServer :: HTTP.Server -> Aff Unit
stopServer = liftEffect <<< flip HTTP.close (pure unit)

type Port = Int


getServerPort :: HTTP.Server -> Aff Net.Address
getServerPort httpserver = do 
  let netserver = unsafeCoerce httpserver
  delay $ Milliseconds 200.00  -- without this delay the os will not have the time to assign a port.
  makeAff \done -> do 
    maddr <- Net.address netserver
    case maddr of 
      Nothing -> done <<< Left $ error "Server is not listening, verify that its properly binded to a port."
      Just addr -> done $ Right $ unsafePartial $ fromLeft $ addr
    pure nonCanceler

withStubbedApi :: Settings -> Application -> (BaseUrl -> Aff Unit) -> Aff Unit
withStubbedApi settings app action =
  bracket (serveStubbedApi settings app)
          stopServer
          (\server -> action =<< (mkBaseUrl <<< _.port) <$> getServerPort server) 
  where 
    mkBaseUrl p = "http://localhost:" <> show p