module Swerve.API.Method where

import Network.HTTP.Types (Method)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPatch, methodPut, methodDelete)
import Swerve.API.Types (Method')
import Type.Proxy (Proxy)


foreign import data GET'    :: Method' 
foreign import data POST'   :: Method' 
foreign import data PUT'    :: Method' 
foreign import data PATCH'  :: Method' 
foreign import data DELETE' :: Method' 

class ToMethod (method :: Method') where 
  toMethod :: Proxy method -> Method

instance getMethod :: ToMethod GET' where  
  toMethod _ = methodGet

instance postMethod :: ToMethod POST' where  
  toMethod _ = methodPost

instance patchMethod :: ToMethod PATCH' where  
  toMethod _ = methodPatch 

instance putMethod :: ToMethod PUT' where  
  toMethod _ = methodPut 

instance deleteMethod :: ToMethod DELETE' where  
  toMethod _ = methodDelete 