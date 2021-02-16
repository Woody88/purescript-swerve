module Swerve.API.Verb where

import Swerve.API.Method (GET', POST', PUT', PATCH', DELETE')

data Verb :: forall k. Type -> Type -> k -> Type
data Verb m cts as 

type Get :: forall k. Type -> k -> Type
type Get cts a    = Verb GET' cts a

type Post :: forall k. Type -> k -> Type
type Post cts a   = Verb POST' cts a

type Put :: forall k. Type -> k -> Type
type Put cts a    = Verb PUT' cts a

type Patch :: forall k. Type -> k -> Type
type Patch cts a  = Verb PATCH' cts a 

type Delete :: forall k. Type -> k -> Type
type Delete cts a = Verb DELETE' cts a