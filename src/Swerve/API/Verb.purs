module Swerve.API.Verb where

import Swerve.API.Status (Ok)
import Swerve.API.Types (Method', Verb)

foreign import data GET'    :: Method'
foreign import data POST'   :: Method'
foreign import data PUT'    :: Method'
foreign import data PATCH'  :: Method'
foreign import data DELETE' :: Method'

type Get a ctypes = Verb GET' a Ok () ctypes
type Get' a status hdrs ctypes = Verb GET' a status hdrs ctypes

type Post a ctypes = Verb POST' a Ok  () ctypes
type Post' a status hdrs ctypes = Verb POST' a status hdrs ctypes

type Put a ctypes = Verb PUT' a Ok  () ctypes
type Put' a status hdrs ctypes = Verb PUT' a status hdrs ctypes 

type Patch a ctypes = Verb PATCH' a Ok  () ctypes 
type Patch' a status hdrs ctypes = Verb PATCH' a status hdrs ctypes 

type Delete a ctypes = Verb DELETE' a Ok  () ctypes 
type Delete' a status hdrs ctypes = Verb DELETE' a status hdrs ctypes
