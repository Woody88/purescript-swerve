module Test.BasicAuth.Example where 

import Prelude 

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Wai (Application)
import Swerve.API (type (:>), BasicAuth, BasicAuthData(..), Get, JSON, Ok, _Ok)
import Swerve.Server (class HasResp, BasicAuthCheck(..), BasicAuthResult(..), Response, Server, respond, serveWithContext)
import Swerve.Server as Server
import Type.Proxy (Proxy(..))

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

type API = BasicAuth "People's websites" User :> "mysite" :> Get Website JSON

site :: forall rs
  .  HasResp Ok () rs 
  => User 
  -> Aff (Response rs Website)
site user = pure $ respond _Ok (website user)

api :: Proxy API
api = Proxy

server :: Server API
server = Server.from site

-- could be a postgres connection, a file, anything.
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

app :: Application
app = serveWithContext api ctx server
  where 
    ctx = { basicAuth: checkBasicAuth userDB }