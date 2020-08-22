module Swerve.API.Spec where 


type Capture record r = (capture :: record | r)

type Query record r = (query :: record | r)

type Header record r = (header :: record | r)

type ReqBody a atype = (reqBody :: a)

type Resource a ctype = (resource :: a)