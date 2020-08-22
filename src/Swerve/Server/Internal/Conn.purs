module Swerver.Server.Internal.Conn where

import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Swerve.API.Verb (Verb)

type ConnectionRow cap qry hdr
  = ( capture :: Record cap
    , query   :: Record qry
    , header  :: Record hdr
    )
    
class Conn specs (conn :: # Type) | specs -> conn 

class HasConn (specs :: RowList) (conn :: # Type) | specs -> conn 

instance conn :: 
  ( HasConn spcl conn 
  , RL.RowToList specs spcl
  ) => Conn (Verb path status t specs) conn

instance hasConnNil :: HasConn RL.Nil conn
else instance hasConnCapture :: HasConn tail rest => HasConn (RL.Cons "capture" ctype tail) (capture :: ctype | rest)
else instance hasConnQuery   :: HasConn tail rest => HasConn (RL.Cons "query" ctype tail) (query :: ctype | rest)
else instance hasConnHeader   :: HasConn tail rest => HasConn (RL.Cons "header" ctype tail) (header :: ctype | rest)