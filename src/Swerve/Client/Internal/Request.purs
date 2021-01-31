module Swerve.Client.Internal.Request where 

import Prelude 

import Data.Maybe 
import Data.Tuple.Nested (type (/\), (/\))
import Network.HTTP.Types as H
import Network.HTTP.Types.Method as H
import Network.HTTP.Media 
import Swerve.API.Header

type RequestRow body path = 
  ( url         :: path
  , queryString :: H.Query
  , body        :: Maybe (body /\ MediaType)
  , accept      :: Array MediaType
  , headers     :: H.RequestHeaders
  , httpVersion :: H.HttpVersion
  , method      :: H.Method
  ) 

type Request = { | RequestRow RequestBody String }

data RequestBody = RequestBodyStr String 

defaultRequest :: Request
defaultRequest = 
  { url: ""
  , queryString: []
  , body: Nothing
  , accept: []
  , headers: []
  , httpVersion: H.http11
  , method: H.methodGet
  }

addHeader :: forall a. ToHeader a => H.HeaderName -> a -> Request -> Request 
addHeader name val req = req { headers = [(name /\ toHeader val)] <> req.headers }