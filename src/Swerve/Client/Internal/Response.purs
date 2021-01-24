module Swerve.Client.Internal.Response where 

import Prelude 

import Debug.Trace
import Data.Either 
import Data.Map as Map 
import Data.Symbol
import Data.Variant (Variant, inj)
import Network.HTTP.Media 
import Network.HTTP.Types as H
import Swerve.API.ContentType
import Swerve.API.Status
import Type.Proxy 
import Type.RowList 
import Type.Row as Row

type ResponseRow a =
  ( status      :: H.Status
  , headers     :: H.ResponseHeaders
  , httpVersion :: H.HttpVersion
  , body        :: a
  )

type Response = { | ResponseRow String }

class AsResponse ctypes (rl :: RowList Type) (row :: Row Type) | rl -> row where 
  mkResponse :: Proxy ctypes -> Proxy rl -> Response -> Either String (Variant row)

instance _asResponse :: 
  ( AllMime ctypes 
  , AllCTUnrender ctypes a
  , MimeUnrender ctypes a
  , HasStatus a status 
  , Row.Cons status a t x
  , IsSymbol status
  ) => AsResponse ctypes (Cons status a Nil) x where 
  mkResponse ctypesP _ res = case reflectSymbol statusP == show res.status.code of
    false -> Left "bad"
    true  -> do 
      let accH = renderHeader $ map show $ allMime ctypesP  
      bodyDecoder <-  note "could not parse accept" $ canHandleCTypeH ctypesP accH 
      let (eBody :: Either String a) = bodyDecoder res.body
      (inj statusP) <$> eBody 
    where 
      statusP = SProxy :: _ status  
      headers = Map.fromFoldable res.headers