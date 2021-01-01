module Swerve.API.SVerb where
  
import Swerve.API.Status 
import Swerve.API.ContentType 

type Ok a r = ("200" :: WithStatus Ok' a | r) 
type BadRequest r = ("400" :: WithStatus BadRequest' String | r)

type Void = ()