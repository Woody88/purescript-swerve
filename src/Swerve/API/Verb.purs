module Swerve.API.Verb where

import Network.HTTP.Types (Method)
import Network.HTTP.Types.Method (methodConnect, methodDelete, methodGet, methodHead, methodOptions, methodPatch, methodPost, methodPut, methodTrace)
import Swerve.API.StatusCode (S200, S201, S202, S203, S204, S205, S206, kind StatusCode)

foreign import kind VERB
foreign import data CONNECT :: VERB
foreign import data HEAD    :: VERB
foreign import data GET     :: VERB
foreign import data POST    :: VERB
foreign import data PATCH   :: VERB
foreign import data PUT     :: VERB
foreign import data DELETE  :: VERB
foreign import data OPTIONS :: VERB
foreign import data TRACE   :: VERB

data Verb (verb :: VERB) (status :: StatusCode) (path :: Symbol) (specs :: # Type)

data VerbP (verb :: VERB) = VerbP

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

type GetNoContent    = Verb GET    S204 
type PostNoContent   = Verb POST   S204 
type PatchtNoContent = Verb PATCH  S204 
type PutNoContent    = Verb PUT    S204 
type DeleteNoContent = Verb DELETE S204 

type GetResetContent    = Verb GET    S205
type PostResetContent   = Verb POST   S205 
type PatchtResetContent = Verb PATCH  S205 
type PutResetContent    = Verb PUT    S205 
type DeleteResetContent = Verb DELETE S205 

type GetPartialContent = Verb GET S206

class ReflectMethod (a :: VERB) where
    reflectMethod :: VerbP a -> Method

instance reflectMtdGet :: ReflectMethod GET where
    reflectMethod _ = methodGet

instance reflectMtdPost :: ReflectMethod POST where
    reflectMethod _ = methodPost

instance reflectMtdPut :: ReflectMethod PUT where
    reflectMethod _ = methodPut

instance reflectMtdDelete :: ReflectMethod DELETE where
    reflectMethod _ = methodDelete

instance reflectMtdPatch :: ReflectMethod PATCH where
    reflectMethod _ = methodPatch

instance reflectMtdHead :: ReflectMethod HEAD where
    reflectMethod _ = methodHead

instance reflectMtdOptions :: ReflectMethod OPTIONS where
    reflectMethod _ = methodOptions

instance reflectMtdTrace :: ReflectMethod TRACE where
    reflectMethod _ = methodTrace

instance reflectMtdConnect :: ReflectMethod CONNECT where
    reflectMethod _ = methodConnect