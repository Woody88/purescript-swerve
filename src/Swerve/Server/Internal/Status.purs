module Swerve.Server.Status where
  
import Swerve.API.Status.Types 
import Swerve.API.Status (WithStatus)

type WithStatusString status = WithStatus status String 

type Ok a r = ("200" :: WithStatus Ok' a | r) 
type Created a r = ("201" :: WithStatus Created' a | r) 
type NoContent r = ("201" :: WithStatusString NoContent' | r)
type MovedPermanently r = ("301" :: WithStatusString MovedPermanently' | r )

type BadRequest r = ("400" :: WithStatusString BadRequest' | r)
type Unauthorized r = ("401" :: WithStatusString Unauthorized' | r)
type Forbidden r = ("403" :: WithStatusString Forbidden' | r)
type NotFound r = ("404" :: WithStatusString NotFound' | r)
type NotImplemented r = ("501" :: WithStatusString NotImplemented' | r)

