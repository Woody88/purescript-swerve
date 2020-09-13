module Swerve.Server.Internal.Response where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (runReaderT)
import Data.Array ((:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Network.HTTP.Types (hAccept, hContentType)
import Network.Wai (Request, Response, Application, responseStr)
import Prim.RowList (class RowToList)
import Swerve.API.ContentTypes (class AllCTRender, AcceptHeader(..), handleAcceptH)
import Swerve.API.Raw (Raw')
import Swerve.API.Spec (Header'(..))
import Swerve.API.StatusCode (class HasStatus, StatusP(..), toStatus)
import Swerve.API.Verb (Verb)
import Swerve.Server.Internal.Handler (HandlerT(..), toParams)
import Swerve.Server.Internal.Header (class ToHeader, toHeader)
import Swerve.Server.Internal.Resource (class Resource)
import Swerve.Server.Internal.ServerError (ServerError, err406)
import Swerver.Server.Internal.Conn (class Conn)
import Type.Equality (class TypeEquals)
import Type.Equality as TEQ
import Type.Proxy (Proxy(..))
  
data SwerveResponse = Rsp Response | Raw Application

class HasResponse handler params m where 
  runHandler :: { | params } -> handler -> Request -> m SwerveResponse

instance hasResponseRaw :: 
  ( Conn (Raw' path specs) params
  , TypeEquals Application waiApplication
  , TypeEquals (Record ()) { | spec }
  , Monad m 
  , MonadThrow ServerError m
  ) => HasResponse (HandlerT (Raw' path specs) m waiApplication) params m where 
  runHandler params (HandlerT handler) req = do
    let verbP = Proxy :: _ (Raw' path specs)
    app <- runReaderT handler (toParams verbP params)
    pure $ Raw (TEQ.from app) 

instance hasResponseHeader ::
  ( Conn (Verb method status path specs) params
  , RowToList specs spcrl 
  , Resource spcrl resp ctype
  , AllCTRender ctype resp 
  , HasStatus status
  , Monad m
  , MonadThrow ServerError m
  , HFoldlWithIndex HeadersUnfold Unit { | hdrs } (Array (Tuple CaseInsensitiveString String))
  ) => HasResponse (HandlerT (Verb method status path specs) m (Header' { | hdrs}   resp)) params m where 
  runHandler params (HandlerT handler) req = do
    let verbP = Proxy :: _ (Verb method status path specs)
    (Header' hdrs resource) <- runReaderT handler (toParams verbP params)
    let accH = getAcceptHeader req
        hdrs' = headersToUnfoldable' hdrs
    case handleAcceptH (Proxy :: _ ctype) accH resource of
      Nothing -> do
        throwError err406
      Just (ct /\ body) -> do 
        pure $ Rsp $ responseStr (toStatus (StatusP :: _ status)) ((hContentType /\ ct) : hdrs') body

else instance hasResponse :: 
  ( Conn (Verb method status path specs) params
  , RowToList specs spcrl 
  , Resource spcrl resp ctype
  , AllCTRender ctype resp 
  , HasStatus status
  , Monad m
  , MonadThrow ServerError m
  ) => HasResponse (HandlerT (Verb method status path specs) m resp) params m where 
  runHandler params (HandlerT handler) req = do
    let verbP = Proxy :: _ (Verb method status path specs)
    resource <- runReaderT handler (toParams verbP params)
    let accH = getAcceptHeader req
    case handleAcceptH (Proxy :: _ ctype) accH resource of
      Nothing -> do
        throwError err406
      Just (ct /\ body) -> do 
        pure $ Rsp $  responseStr (toStatus (StatusP :: _ status)) [hContentType /\ ct] body

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