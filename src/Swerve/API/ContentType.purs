module Swerve.API.ContentType 
    (
    -- * Provided Content-Types
      JSON
    , PlainText
    , FormUrlEncoded

    -- * Building your own Content-Type
    , class Accept
    , class MimeRender
    , class MimeUnrender

    -- * NoContent
    , NoContent(..)

    -- * Internal
    , AcceptHeader (..)
    , class AllCTRender
    , class AllCTUnrender
    , class AllMime
    , class AllMimeRender
    , class AllMimeUnrender
    , allMime
    , allMimeRender
    , allMimeUnrender
    , canHandleAcceptH
    , canHandleCTypeH
    , contentType
    , contentTypes
    , handleAcceptH
    , mimeRender
    , mimeUnrender
    )
    where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, printJsonDecodeError, parseJson, decodeJson, encodeJson, stringify)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded)
import Data.FormURLEncoded (encode) as FormURLEncoded
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import Foreign as Foreign
import Network.HTTP.Media (MediaType, (//))
import Network.HTTP.Media as H
import Network.HTTP.Media as M
import Network.HTTP.Media as Media
import Prim.TypeError as TE
import Swerve.API.Alternative (type (:))
import Swerve.API.Status (WithStatus(..))
import Swerve.API.Status.Types (NoContent')
import Type.Proxy (Proxy(..))

data Star 
data JSON 
data PlainText 
data FormUrlEncoded 

data NoContent = NoContent 

newtype AcceptHeader = AcceptHeader String 

-- Need to review this 
-- instance writeForeignNoContent :: WriteForeign NoContent where 
--     writeImpl _ = Json.undefined

derive instance eqAcceptHeader :: Eq AcceptHeader

class Accept :: forall k. k -> Constraint
class Accept ctype where 
  contentTypes  :: Proxy ctype -> NonEmptyArray MediaType
  contentType :: Proxy ctype -> MediaType


instance acceptJson :: Accept JSON where 
    contentType _ = "application" // "json"

    contentTypes p = NE.singleton $ contentType p

instance acceptPlainText :: Accept PlainText where 
    contentType _ = "text" // "plain"
    
    contentTypes p = NE.singleton $ contentType p

instance acceptFormUrlEncoded :: Accept FormUrlEncoded where 
    contentType _ = "application" // "x-www-form-urlencoded"

    contentTypes p = NE.singleton $ contentType p

class AllCTRender :: forall k. k -> Type -> Constraint
class AllMime ctypes <= AllCTRender ctypes a where
    handleAcceptH :: Proxy ctypes -> AcceptHeader -> a -> Maybe (Tuple String String)

instance allCTRenderUnit :: 
    (TE.Fail (TE.Text "No instance for Unit, use NoContent instead.")
    , Accept ctype
    ) => AllCTRender ctype Unit where
    handleAcceptH _ _ _ = Nothing

else instance allCTRender' :: 
    ( AllMime cts
    , AllMimeRender cts a
    ) => AllCTRender cts a where
    handleAcceptH pctyps (AcceptHeader accept) val = Media.mapAcceptMedia lkup accept
        where 
            amrs = allMimeRender pctyps val
            lkup = map (\(Tuple a b) -> Tuple a (Tuple (Media.renderHeader a) b)) amrs

class MimeRender :: forall k. k -> Type -> Constraint
class Accept ctype <= MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> String 

instance mimeRenderWithStatusJsob :: MimeRender JSON a => MimeRender JSON (WithStatus status a) where
    mimeRender p (WithStatus _ a) = mimeRender p a 

else instance mimeRenderJson :: EncodeJson a => MimeRender JSON a where 
    mimeRender _ = stringify <<< encodeJson

instance mimeRenderWithStatusText :: MimeRender PlainText a => MimeRender PlainText (WithStatus status a) where
    mimeRender p (WithStatus _ a) = mimeRender p a 

instance mimeRenderPlainTextString :: MimeRender PlainText String where 
    mimeRender _ = identity

instance mimeRenderPlainTextNoContent :: MimeRender PlainText NoContent where 
    mimeRender _ = mempty

-- Need to review this 
-- instance mimeRenderPlainTextRecord :: WriteForeign (Record r) => MimeRender PlainText (Record r) where 
--     mimeRender _ = Json.writeJSON

instance mimeRenderFormUrlEncoded :: MimeRender FormUrlEncoded FormURLEncoded where 
    mimeRender _ = fromMaybe mempty <<< FormURLEncoded.encode

class AllMime :: forall k. k -> Constraint
class AllMime ctypes where 
    allMime :: Proxy ctypes -> Array MediaType 

instance allMimeAlt :: (AllMime ctypes, Accept ctype, Accept ctypes)  => AllMime (ctype : ctypes) where 
    allMime _ =  Array.cons (contentType pctype) $ allMime pctypes
        where 
            pctype  = Proxy :: Proxy ctype 
            pctypes = Proxy :: Proxy ctypes

else instance allMimeBase :: Accept ctype => AllMime ctype where 
    allMime ctype = [contentType ctype]

class AllMimeRender :: forall k. k -> Type -> Constraint
class AllMimeRender ctype a where
    allMimeRender :: Proxy ctype -> a -> Array (Tuple MediaType String)

instance allMimeAltNoContent :: (AllMime ctypes) => AllMimeRender ctypes NoContent where 
    allMimeRender pxy x = let arr = (allMime pxy) 
    in Array.zip arr $ Array.replicate (Array.length arr) ""
        where 
            pxys = Proxy :: Proxy ctypes  

else instance allMimeRenderAlt :: (AllMimeRender ctypes a, Accept ctype, MimeRender ctype a) => AllMimeRender (ctype : ctypes) a where 
    allMimeRender _ x = Array.cons (Tuple (contentType pxy) (mimeRender pxy x)) $ allMimeRender pxys x
        where 
            pxy  = Proxy :: Proxy ctype 
            pxys = Proxy :: Proxy ctypes  

else instance allMimeRender' :: (Accept ctype, MimeRender ctype a) => AllMimeRender ctype a where  --- base case  type class 
    allMimeRender pxy x = [ Tuple (contentType pxy) (mimeRender pxy x) ]

class MimeUnrender :: forall k. k -> Type -> Constraint
class Accept ctype <= MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> String -> Either String a    

instance mimeUnrenderWithStatusNoContent :: MimeUnrender JSON (WithStatus NoContent' String) where  
    mimeUnrender p input = pure $ WithStatus Proxy mempty

else instance mimeUnrenderWithStatusJson :: MimeUnrender JSON a => MimeUnrender JSON (WithStatus status a) where  
    mimeUnrender p input = WithStatus Proxy <$> mimeUnrender p input

else instance mimeUnrenderJson :: DecodeJson a => MimeUnrender JSON a where 
    mimeUnrender _ s = lmap printJsonDecodeError $ decodeJson =<< parseJson s

instance mimeUnrenderPlainText ::  MimeUnrender PlainText String where 
    mimeUnrender _ x = Right x

class AllMimeUnrender :: forall k. k -> Type -> Constraint
class (AllMime list) <= AllMimeUnrender list a where
  allMimeUnrender :: Proxy list -> Array (Tuple M.MediaType (String -> Either String a))

instance allMimeUnrenderAlt :: 
  ( MimeUnrender ctyp a
  , Accept ctyps
  , AllMimeUnrender ctyps a
  ) => AllMimeUnrender (ctyp : ctyps) a where
  allMimeUnrender _ =
    map mk (NE.toArray $ contentTypes pctyp) <> allMimeUnrender pctyps
    where
      mk ct   = Tuple ct (mimeUnrenderWithType pctyp ct)
      pctyp  = Proxy :: Proxy ctyp
      pctyps = Proxy :: Proxy ctyps

else instance allMimeUnrender' :: 
  ( MimeUnrender ctyp a
  , Accept ctyp
  ) => AllMimeUnrender ctyp a where
  allMimeUnrender _ = map mk (NE.toArray $ contentTypes pctyp) 
    where
      mk ct   = Tuple ct (mimeUnrenderWithType pctyp ct)
      pctyp  = Proxy :: Proxy ctyp

class AllCTUnrender :: forall k. k -> Type -> Constraint
class AllCTUnrender list a where
  canHandleCTypeH
    :: Proxy list
    -> String  -- Content-Type header
    -> Maybe (String -> Either String a)

instance allCTUnrender :: AllMimeUnrender ctyps a => AllCTUnrender ctyps a where
  canHandleCTypeH p ctypeH = H.mapContentMedia (allMimeUnrender p) ctypeH

mimeUnrenderWithType :: forall ctype a. MimeUnrender ctype a => Proxy ctype -> M.MediaType -> String -> Either String a
mimeUnrenderWithType p _ = mimeUnrender p

canHandleAcceptH :: forall list. AllMime list => Proxy list -> AcceptHeader -> Boolean 
canHandleAcceptH p (AcceptHeader h) = isJust $ M.matchAccept (allMime p) h