module Swerve.API.Verb where

import Swerve.API.Method (GET', POST', PUT', PATCH', DELETE')
import Swerve.API.Types (Method', ContentType')

data Verb :: forall k. Method' -> ContentType' -> k -> Type
data Verb m cts as 

type Get :: forall k. k -> ContentType' -> Type
type Get cts a    = Verb GET' a cts

type Post :: forall k. k -> ContentType' -> Type
type Post cts a   = Verb POST' a cts

type Put :: forall k. k -> ContentType' -> Type
type Put cts a    = Verb PUT' a cts

type Patch :: forall k. k -> ContentType' -> Type
type Patch cts a  = Verb PATCH' a cts 

type Delete :: forall k. k -> ContentType' -> Type
type Delete cts a = Verb DELETE' a cts