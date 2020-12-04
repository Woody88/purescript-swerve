module Server.Internal.BasicAuth where

import Prelude 
import Control.MonadZero (guard)
import Data.Either (hush)
import Data.String.Base64 as Base64
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Data.String (indexOf, length, toLower, trim) as String
import Data.String.CodeUnits (slice) as String 
import Data.String.Utils (words) as String 
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Network.HTTP.Types (Header) as H
import Network.Wai (Request)
import Swerve.API.BasicAuth (BasicAuthData(..))
import Swerve.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal)
import Swerve.Server.Internal.ServerError (err401, err403)

data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr

derive instance ordBasicAuthResult :: Ord usr => Ord (BasicAuthResult usr) 
derive instance eqBasicAuthResult  :: Eq usr => Eq  (BasicAuthResult usr)
derive instance functorBasicAuthResult :: Functor BasicAuthResult 

newtype BasicAuthCheck usr = BasicAuthCheck (BasicAuthData -> Aff (BasicAuthResult usr))

derive instance functorBasicAuthCheck :: Functor BasicAuthCheck

mkBAChallengerHeader :: String -> H.Header
mkBAChallengerHeader realm = Tuple (wrap "WWW-Authenticate") ("Basic realm=\"" <> realm <> "\"")

decodeBAHeader :: Request -> Maybe BasicAuthData
decodeBAHeader req = do
  ah        <- Map.lookup (wrap "Authorization") requestHeaders 
  (b /\ rest) <-  case String.words ah of
                  [b, rest] -> pure $ b /\ rest 
                  otherwise -> Nothing

  guard (String.toLower b == "basic")

  decoded  <- hush <<< Base64.decode $ String.trim rest
  username <- String.indexOf (wrap colon) decoded >>= \i -> String.slice 0 i decoded
  password <- String.indexOf (wrap colon) decoded >>= \i -> String.slice (i + 1) (String.length decoded) decoded
  pure $ BasicAuthData { username, password }
  
  where 
    requestHeaders = Map.fromFoldable <<< _.headers $ unwrap req
    colon = ":"

runBasicAuth :: forall usr. Request -> String-> BasicAuthCheck usr -> DelayedIO usr
runBasicAuth req realm (BasicAuthCheck ba) = case decodeBAHeader req of
  Nothing -> plzAuthenticate
  Just e  -> liftAff (ba e) >>= \res -> case res of
    BadPassword    -> plzAuthenticate
    NoSuchUser     -> plzAuthenticate
    Unauthorized   -> delayedFailFatal err403
    Authorized usr -> pure usr

  where 
    plzAuthenticate = delayedFailFatal err401 { headers = [mkBAChallengerHeader realm] }