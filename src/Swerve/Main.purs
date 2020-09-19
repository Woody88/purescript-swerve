module Main where

import Prelude

import Data.Symbol (SProxy(..))
import Record.Builder as Builder

x = Builder.insert (SProxy :: _ "hello") identity

y = Builder.modify (SProxy :: _ "capture") (\a -> (Builder.insert (SProxy :: _ "x") 1) <<< a)

