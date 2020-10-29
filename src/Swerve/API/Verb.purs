module Swerve.API.Verb where

import Swerve.API.Status (Ok)
import Swerve.API.Types (Verb)

type Get a ctypes = Verb Ok a () ctypes () 
type Get' a ctypes hdrs resp = Verb Ok a hdrs ctypes resp