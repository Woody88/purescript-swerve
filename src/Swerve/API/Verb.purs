module Swerve.API.Verb where

import Swerve.API.Method (GET', POST', PUT', PATCH', DELETE')
import Swerve.API.Types (Method', ContentType')

data Verb :: forall k. Method' -> ContentType' -> k -> Type
data Verb m cts as 

type Get :: forall k. ContentType' -> k -> Type
type Get cts a    = Verb GET' cts a

type Post :: forall k. ContentType' -> k -> Type
type Post cts a   = Verb POST' cts a

type Put :: forall k. ContentType' -> k -> Type
type Put cts a    = Verb PUT' cts a

type Patch :: forall k. ContentType' -> k -> Type
type Patch cts a  = Verb PATCH' cts a 

type Delete :: forall k. ContentType' -> k -> Type
type Delete cts a = Verb DELETE' cts a