module Swerve.API.ContentTypes where 

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded)
import Data.FormURLEncoded (encode) as FormURLEncoded
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.MediaType (MediaType)
import Data.MediaType.Common as Media
import Foreign as Foreign
import Prim.RowList as RL
import Prim.TypeError as TE
import Simple.JSON as Json
import Swerve.API.Combinators (type (:<|>))
import Type.Proxy (Proxy(..))

data JSON 
data PlainText 
data FormUrlEncoded 
data OctetStream 

data NoContent = NoContent

type Accept a r = ("accept" :: a | r)
type ContentType a r = ("content-type" :: a | r)

class Accepts ctype where 
    contentType :: Proxy ctype -> MediaType

instance acceptJson :: Accepts JSON where 
    contentType _ = Media.applicationJSON 

instance acceptPlainText :: Accepts PlainText where 
    contentType _ = Media.textPlain

instance acceptFormUrlEncoded :: Accepts FormUrlEncoded where 
    contentType _ = Media.applicationFormURLEncoded

class Accepts ctype <= MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> String 

instance mimeRenderJson :: Json.WriteForeign a => MimeRender JSON a where 
    mimeRender _ = Json.writeJSON

instance mimeRenderPlainTextString :: MimeRender PlainText String where 
    mimeRender _ = identity

else instance mimeRenderFormUrlEncoded :: MimeRender FormUrlEncoded FormURLEncoded where 
    mimeRender _ = fromMaybe mempty <<< FormURLEncoded.encode

else instance mimeRenderPlainText :: Show a => MimeRender PlainText a where 
    mimeRender _ = show

class AllMimeRender ctype a where
    allMimeRender :: Proxy ctype -> a -> Map MediaType String 


instance allMimeRenderAlt :: (AllMimeRender ctypes a, Accepts ctype, MimeRender ctype a) => AllMimeRender (ctype :<|> ctypes) a where 
    allMimeRender _ x = Map.union (Map.singleton (contentType pxy) (mimeRender pxy x)) (allMimeRender pxys x)
        where 
            pxy  = Proxy :: Proxy ctype 
            pxys = Proxy :: Proxy ctypes  

else instance allMimeRender' :: (Accepts ctype, MimeRender ctype a) => AllMimeRender ctype a where  --- base case  type class 
    allMimeRender pxy x = Map.singleton (contentType pxy) (mimeRender pxy x)

class Accepts ctype <= MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> String -> Either String a    

instance mimeUnrenderJson :: Json.ReadForeign a => MimeUnrender JSON a where 
    mimeUnrender _ = lmap renderError <<< Json.readJSON 
        where 
            renderError :: Foreign.MultipleErrors -> String 
            renderError = show <<< map Foreign.renderForeignError

instance mimeUnrenderPlainText :: Show a => MimeUnrender PlainText String where 
    mimeUnrender _ = Right <<< show 


