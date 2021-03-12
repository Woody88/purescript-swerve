module Test.API.Auth where 

import Prelude 

import Control.Monad.Except.Trans (class MonadThrow, ExceptT(..), runExceptT, throwError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(..))
import Data.String (indexOf, length) as String
import Data.String.CodeUnits (slice) as String 
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Wai (Application, Request(..))
import Swerve.API
import Swerve.Server
import Swerve.Server as Server
import Type.Proxy (Proxy(..)) 

newtype Account = Account String 

account :: Account -> String 
account (Account acc) = acc 

database :: Map String Account
database = Map.fromFoldable users

users :: Array (Tuple String Account)
users = [ Tuple "key1" (Account "Anne Briggs")
        , Tuple "key2" (Account "Bruce Cockburn")
        , Tuple "key3" (Account "Ghédalia Tazartès")
        ]
      
lookupAccount :: forall m
  .  MonadThrow String m 
  => String -> m Account
lookupAccount key = case Map.lookup key database of
  Nothing -> throwError "Invalid Cookie"
  Just usr -> pure $ usr

authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
  maybeToEither e = maybe (Left e) Right
  requestHeaders = Map.fromFoldable <<< _.headers <<< unwrap 

  handler :: Request -> Aff (Either ServerError Account)
  handler req@(Request r) = do 
    e <- runExceptT do 
          name <- ExceptT $ pure do
            (maybeToEither "Missing cookie header" $ Map.lookup (wrap "X-Cookie") $ requestHeaders req) 
              >>= (maybeToEither "Missing swerve auth cookie" <<< Map.lookup "swerve-auth-cookie" <<< Map.fromFoldable <<< parseCookies)
          lookupAccount name
    pure $ lmap (\msg -> err403 { content = msg}) e

-- naive cookie parsing... Only for example purpose
parseCookies :: String -> Array (Tuple String String)
parseCookies s
  | Just i    <- String.indexOf (Pattern "=") s 
  , Just name <- String.slice 0 i s 
  , Just val  <- String.slice (i + 1) (String.length s) s = [Tuple name val]
  | otherwise = []
  
type AuthGenAPI = Record 
  ( private :: "private" :> AuthProtect "cookie-auth" :> PrivateAPI
  , public  :: "public"  :> PublicAPI
  )

type PrivateAPI = "secret" :> Get JSON (Ok String + Nil)
type PublicAPI  = "data"   :> Get JSON (Ok String + Nil)

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

server :: Server AuthGenAPI
server = Server.lift { private, public }
  where 
    private :: Account -> Handler (Ok String + Nil)
    private (Account name) = do 
      pure <<< respond (Proxy :: _ Ok') $ ("this is a secret: " <> name)
    
    public :: Handler (Ok String + Nil)
    public = pure <<< respond (Proxy :: _ Ok') $ "this is a public piece of data"

-- Convert our endpoint specification into a Wai Application that can be run by Warp.
app :: Application
app = serveWithContext genAuthAPI ctx server
  where 
    ctx = { "cookie-auth": authHandler }