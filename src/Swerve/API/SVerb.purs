module Swerve.API.SVerb where
  
import Swerve.API.Status 
import Swerve.API.ContentType 

type Ok a =  WithStatus Ok' a 
type BadRequest = WithStatus BadRequest' NoContent