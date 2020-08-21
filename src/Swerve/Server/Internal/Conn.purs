module Swerver.Server.Internal.Conn where

import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Swerve.API.Verb (Verb)

class Conn specs (conn :: # Type) | specs -> conn 

class HasConn (specs :: RowList) (conn :: # Type) | specs -> conn 

instance conn :: 
  ( HasConn spcl conn 
  , RL.RowToList specs spcl
  ) => Conn (Verb path status t specs) conn

instance hasConnNil :: HasConn RL.Nil conn
else instance hasConnCapture :: HasConn tail rest => HasConn (RL.Cons "capture" ctype tail) (capture :: ctype | rest)