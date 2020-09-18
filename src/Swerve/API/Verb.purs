module Swerve.API.Verb where

import Network.HTTP.Types.Method (Method, methodConnect, methodDelete, methodGet, methodHead, methodOptions, methodPatch, methodPost, methodPut, methodTrace) as H
import Swerve.API.StatusCode (S200, S201, S202, S203, S205, S206, StatusCode)
import Type.Proxy (Proxy)

data Method 
foreign import data CONNECT :: Method
foreign import data HEAD    :: Method
foreign import data GET     :: Method
foreign import data POST    :: Method
foreign import data PATCH   :: Method
foreign import data PUT     :: Method
foreign import data DELETE  :: Method
foreign import data OPTIONS :: Method
foreign import data TRACE   :: Method

data Verb (method :: Method) (status :: StatusCode) (path :: Symbol)

data NoContentVerb  (method :: Method)

type Get     = Verb GET    S200
type Post    = Verb POST   S200 
type Patch   = Verb PATCH  S200 
type Put     = Verb PUT    S200 
type Delete  = Verb DELETE S200 

type PostCreated = Verb POST S201
type PutCreated  = Verb PUT  S201

type GetAccepted    = Verb GET    S202
type PostAccepted   = Verb POST   S202 
type PatchtAccepted = Verb PATCH  S202 
type PutAccepted    = Verb PUT    S202 
type DeleteAccepted = Verb DELETE S202 

type GetNonAuthoritative    = Verb GET    S203
type PostNonAuthoritative   = Verb POST   S203 
type PatchtNonAuthoritative = Verb PATCH  S203 
type PutNonAuthoritative    = Verb PUT    S203 
type DeleteNonAuthoritative = Verb DELETE S203 

type GetNoContent    = NoContentVerb GET    
type PostNoContent   = NoContentVerb POST   
type PatchtNoContent = NoContentVerb PATCH  
type PutNoContent    = NoContentVerb PUT    
type DeleteNoContent = NoContentVerb DELETE 

type GetResetContent    = Verb GET    S205
type PostResetContent   = Verb POST   S205 
type PatchtResetContent = Verb PATCH  S205 
type PutResetContent    = Verb PUT    S205 
type DeleteResetContent = Verb DELETE S205 

type GetPartialContent = Verb GET S206

class ReflectMethod (a :: Method) where
    reflectMethod :: Proxy a -> H.Method

instance reflectMtdGet :: ReflectMethod GET where
    reflectMethod _ = H.methodGet

instance reflectMtdPost :: ReflectMethod POST where
    reflectMethod _ = H.methodPost

instance reflectMtdPut :: ReflectMethod PUT where
    reflectMethod _ = H.methodPut

instance reflectMtdDelete :: ReflectMethod DELETE where
    reflectMethod _ = H.methodDelete

instance reflectMtdPatch :: ReflectMethod PATCH where
    reflectMethod _ = H.methodPatch

instance reflectMtdHead :: ReflectMethod HEAD where
    reflectMethod _ = H.methodHead

instance reflectMtdOptions :: ReflectMethod OPTIONS where
    reflectMethod _ = H.methodOptions

instance reflectMtdTrace :: ReflectMethod TRACE where
    reflectMethod _ = H.methodTrace

instance reflectMtdConnect :: ReflectMethod CONNECT where
    reflectMethod _ = H.methodConnect