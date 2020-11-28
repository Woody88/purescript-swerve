module Swerve.API.Verb where

import Swerve.API.Types (Method', Verb)

foreign import data GET'  :: Method'
foreign import data POST' :: Method'

type Get a status hdrs ctypes = Verb GET' a status hdrs ctypes 
-- type Get' a ctypes hdrs resp = Verb Ok a hdrs ctypes resp

type Post a status hdrs ctypes = Verb POST' a status hdrs ctypes 
-- type Post' a ctypes hdrs resp = Verb Ok a hdrs ctypes resp

-- type Put a ctypes = Verb Ok a () ctypes () 
-- type Put' a ctypes hdrs resp = Verb Ok a hdrs ctypes resp

-- type Patch a ctypes = Verb Ok a () ctypes () 
-- type Patch' a ctypes hdrs resp = Verb Ok a hdrs ctypes resp

-- type Delete a ctypes = Verb Ok a () ctypes () 
-- type Delete' a ctypes hdrs resp = Verb Ok a hdrs ctypes resp