module Swerve.API.Method where

import Network.HTTP.Types (Method)
import Network.HTTP.Types.Method (methodGet, methodPost, methodPatch, methodPut, methodDelete)
import Type.Proxy (Proxy)


data GET'    
data POST'   
data PUT'    
data PATCH'  
data DELETE' 

class ToMethod method where 
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