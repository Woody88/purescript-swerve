module Swerve.Server.Internal where

import Type.Data.Row (RProxy)

type Server (api :: # Type) = RProxy 

class HasServer api 