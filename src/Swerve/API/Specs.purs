module Swerve.API.Spec where

import Prelude

import Type.Proxy (Proxy)

type Capture record r = (capture :: record | r)

type Query record r = (query :: record | r)

type Header record r = (header :: record | r)

type ReqBody a ctype r = (body :: ReqBody' a ctype | r)

type ContentType ctype r = (contentType :: Proxy ctype  | r)

type Resource a ctype r = (resource :: Resource' a ctype | r)

newtype ReqBody' a ctype = ReqBody' a 

newtype Resource' a ctype = Resource' a

data Header' record a  = Header' record a