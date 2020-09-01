module Swerve.Server.Internal.Response where

import Prelude

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Reader (runReaderT)
import Data.Array ((:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Network.HTTP.Types (hAccept, hContentType, notAcceptable406)
import Network.Wai (Request, Response, responseStr)
import Prim.RowList (class RowToList)
import Swerve.API.ContentTypes (class AllCTRender, AcceptHeader(..), handleAcceptH)
import Swerve.API.Spec (Header'(..))
import Swerve.API.StatusCode (class HasStatus, StatusP(..), toStatus)
import Swerve.API.Verb (Verb)
import Swerve.Server.Internal.Handler (Handler(..), toParams)
import Swerve.Server.Internal.Resource (class Resource)
import Swerve.Server.Internal.ParseHeader (class ToHeader, toHeader)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Proxy (Proxy(..))

class HasResponse handler params where 
  runHandler :: { | params } -> handler -> Request -> ExceptT String Aff Response

instance hasResponseHeader ::
  ( Conn (Verb method status path specs) params
  , RowToList specs spcrl 
  , Resource spcrl resp ctype
  , AllCTRender ctype resp 
  , HasStatus status
  , HFoldlWithIndex HeadersUnfold Unit { | hdrs } (Array (Tuple CaseInsensitiveString String))
  ) => HasResponse (Handler (Verb method status path specs) (Header' { | hdrs}   resp)) params where 
  runHandler params (Handler handler) req = do
    let verbP = Proxy :: _ (Verb method status path specs)
    (Header' hdrs resource) <- runReaderT handler (toParams verbP params)
    let accH = getAcceptHeader req
        hdrs' = headersToUnfoldable' hdrs
    case handleAcceptH (Proxy :: _ ctype) accH resource of
      Nothing -> do
        throwError notAcceptable406.message
      Just (ct /\ body) -> do 
        pure $ responseStr (toStatus (StatusP :: _ status)) ((hContentType /\ ct) : hdrs') body

else instance hasResponse :: 
  ( Conn (Verb method status path specs) params
  , RowToList specs spcrl 
  , Resource spcrl resp ctype
  , AllCTRender ctype resp 
  , HasStatus status
  ) => HasResponse (Handler (Verb method status path specs) resp) params where 
  runHandler params (Handler handler) req = do
    let verbP = Proxy :: _ (Verb method status path specs)
    resource <- runReaderT handler (toParams verbP params)
    let accH = getAcceptHeader req
    case handleAcceptH (Proxy :: _ ctype) accH resource of
      Nothing -> do
        throwError notAcceptable406.message
        -- resp $ responseStr notAcceptable406 [] notAcceptable406.message
      Just (ct /\ body) -> do 
        pure $ responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body

getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader = AcceptHeader <<< fromMaybe ct_wildcard <<< Map.lookup hAccept <<< Map.fromFoldable <<< _.headers <<< unwrap

ct_wildcard :: String
ct_wildcard = "*" <> "/" <> "*"

data HeadersUnfold = HeadersUnfold

instance headersUnfoldUnit ::
  ( IsSymbol sym
  , ToHeader a
  ) => FoldingWithIndex HeadersUnfold (SProxy sym) Unit a (Array (Tuple CaseInsensitiveString String))
  where
  foldingWithIndex _ prop _ a = ((wrap $ reflectSymbol prop) /\ (toHeader a)) : []

instance headersUnfold ::
  ( IsSymbol sym
  , ToHeader a
  ) => FoldingWithIndex HeadersUnfold (SProxy sym) (Array (Tuple CaseInsensitiveString String)) a (Array (Tuple CaseInsensitiveString String))
  where
  foldingWithIndex _ prop hdrs a =  ((wrap $ reflectSymbol prop) /\ (toHeader a)) : hdrs

headersToUnfoldable' :: forall r.
  HFoldlWithIndex HeadersUnfold Unit { | r } (Array (Tuple CaseInsensitiveString String)) 
  => { | r } 
  -> (Array (Tuple CaseInsensitiveString String))
headersToUnfoldable' r = hfoldlWithIndex HeadersUnfold unit r 