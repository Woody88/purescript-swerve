module Test.API.Verb where 

import Prelude

import Network.Wai (Application) as Wai
import Swerve.API (type (:>), Delete, Get, PlainText, JSON, Ok', Patch, Post, Put, ReqBody)
import Swerve.Server (type (+), Handler, Nil, Ok, Server, respond, serve)
import Swerve.Server (lift) as Server
import Type.Proxy (Proxy(..))

type VerbApi = Record 
  ( get        :: "get"    :> Get    JSON (Ok String + Nil)
  , post       :: "post"   :> ReqBody PlainText String :> Post JSON (Ok String + Nil)
  , postNoBody :: "post-no-body" :> Post JSON (Ok String + Nil)
  , patch      :: "patch"  :> Patch  JSON (Ok String + Nil)
  , put        :: "put"    :> Put    JSON (Ok String + Nil)
  , delete     :: "delete" :> Delete JSON (Ok String + Nil)
  )

get :: Handler (Ok String + Nil) 
get = pure <<< respond (Proxy :: _ Ok') $  "get"

post :: String -> Handler (Ok String + Nil) 
post _ = pure <<< respond (Proxy :: _ Ok') $  "post"

postNoBody :: Handler (Ok String + Nil)
postNoBody = pure <<< respond (Proxy :: _ Ok') $ "postNoBody"

patch :: Handler (Ok String + Nil)
patch = pure <<< respond (Proxy :: _ Ok') $ "patch"

put :: Handler (Ok String + Nil)
put = pure <<< respond (Proxy :: _ Ok') $ "put"

delete :: Handler (Ok String + Nil)
delete = pure <<< respond (Proxy :: _ Ok') $ "delete"

server :: Server VerbApi
server = Server.lift { get, post, postNoBody, patch, put, delete }

app :: Wai.Application
app = serve (Proxy :: _ VerbApi) server
