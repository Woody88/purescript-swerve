module Swerve.API.Method where

import Network.HTTP.Types (Method)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Swerve.API.Types (Method')
import Swerve.API.Verb (GET', POST')
import Type.Proxy (Proxy)

class ToMethod (method :: Method') where 
  toMethod :: Proxy method -> Method

instance getMethod :: ToMethod GET' where  
  toMethod _ = methodGet

instance postMethod :: ToMethod POST' where  
  toMethod _ = methodPost