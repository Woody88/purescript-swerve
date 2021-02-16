module Swerve.Client.Internal.Response where 

import Prelude 

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

instance _asResponseNil :: 
  ( AllMime ctypes 
  , AllCTUnrender ctypes a
  , MimeUnrender ctypes a
  , HasStatus a status 
  , Row.Cons status a r row
  , IsSymbol status
  ) => AsResponse ctypes (Cons status a Nil) row where 
  mkResponse ctypesP _ res = case reflectSymbol statusP == show res.status.code of
    false -> Left $ "wrong status code: " <> reflectSymbol statusP  <> " =/ " <> show res.status.code
    true  -> do 
      let accH = renderHeader $ map show $ allMime ctypesP  
      bodyDecoder <-  note "could not parse accept" $ canHandleCTypeH ctypesP accH 
      let (eBody :: Either String a) = bodyDecoder res.body
      (inj statusP) <$> eBody 
    where 
      statusP = SProxy :: _ status  
      headers = Map.fromFoldable res.headers

else instance _asResponse :: 
  ( AllMime ctypes 
  , AllCTUnrender ctypes a
  , MimeUnrender ctypes a
  , HasStatus a status 
  , Row.Cons status a r to
  , IsSymbol status
  , AsResponse ctypes rest to
  ) => AsResponse ctypes (Cons status a rest) to where 
  mkResponse ctypesP _ res = case reflectSymbol statusP == show res.status.code of
    false -> mkResponse ctypesP (Proxy :: _ rest) res -- Left $ "wrong status code: " <> reflectSymbol statusP  <> " =/ " <> show res.status.code -- expand <$> mkResponse ctypesP (Proxy :: _ rest) res 
    true  -> do 
      let accH = renderHeader $ map show $ allMime ctypesP  
      bodyDecoder <- note "could not parse accept" $ canHandleCTypeH ctypesP accH 
      let (eBody :: Either String a) = bodyDecoder res.body
      (inj statusP) <$> eBody 
    where 
      statusP = SProxy :: _ status  
      headers = Map.fromFoldable res.headers