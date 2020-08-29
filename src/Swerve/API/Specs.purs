module Swerve.API.Spec where

import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))

type Capture record r = (capture :: record | r)

type Query record r = (query :: record | r)

type Header record r = (header :: record | r)

-- type ReqBody a ctype r = (body :: a, contentType :: ctype | r)
type ReqBody a ctype r = (body :: a, contentType :: Proxy ctype  | r)

type ContentType ctype r = (contentType :: Proxy ctype  | r)

type Resource a ctype = (resource :: a)

newtype ReqBody' a ctype = ReqBody' a 

derive instance newtypeReqBody :: Newtype (ReqBody' a ctype) _