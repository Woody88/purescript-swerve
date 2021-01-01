module Swerve.Server.Status where
  
import Swerve.API.Status (WithStatus, Ok', BadRequest')

type Void :: forall k. Row k
type Void = ()

type Ok a r = ("200" :: WithStatus Ok' a | r) 
type BadRequest r = ("400" :: WithStatus BadRequest' String | r)

