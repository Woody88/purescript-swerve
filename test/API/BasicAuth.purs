module Test.API.BasicAuth where 

import Prelude 

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.Wai (Application)
import Swerve.API 
import Swerve.Server 
import Swerve.Server as Server
import Type.Proxy (Proxy(..))

-- Defining a User type that is required by API service. 
type Username = String
type Password = String
type Website  = String

data User = User Username Password Website

username :: User -> Username 
username (User uname _ _) = uname

pass :: User -> Password 
pass (User _ p _) = p

website :: User -> Website 
website (User _ _ w) = w

derive instance ordUser  :: Ord User
derive instance eqUser   :: Eq User
instance showUser :: Show User where 
  show (User u p w) = "(User \"" <> show u <> " \"" <> show p <> " \"" <> show w <> "\"" 

-- Defining API spec and handler 
type API = Record 
  ( site :: BasicAuth "People's websites" User :> "mysite" :> Get JSON (Ok Website + Nil)
  )

site :: User -> Handler (Ok Website + Nil)
site = pure <<< respond (Proxy :: _ Ok') <<< website 

api :: Proxy API
api = Proxy

server :: Server API
server = Server.lift { site }

-- Defining Database 
type UserDB = Map.Map Username User

-- create a "database" from a list of users
createUserDB :: Array User -> UserDB
createUserDB = Map.fromFoldable <<< map (\u -> Tuple (username u) u)

-- our test database
userDB :: UserDB
userDB = createUserDB users

users :: Array User 
users =
  [ User "john" "shhhh" "john.com"
  , User "foo" "bar" "foobar.net"
  ]

-- Defining context that determines what is a valid User. 
-- This can also be thought of as a handler that contains the compuation that knows how to validate a user
-- If it fails swerve will return an appropriate response based on BasicAuthCheck result (ie: NoUser, BadPassword, etc...).
checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db = BasicAuthCheck $ \(BasicAuthData basicAuthData) ->
  let username' = basicAuthData.username
      password = basicAuthData.password
  in
  case Map.lookup username' db of
    Nothing -> pure NoSuchUser
    Just u  -> if pass u == password
               then pure (Authorized u)
               else pure BadPassword

-- Convert our endpoint specification into a Wai Application that can be run by Warp.
app :: Application
app = serveWithContext api ctx server
  where 
    ctx = { basicAuth: checkBasicAuth userDB }