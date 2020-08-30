module Swerver.Server.Internal.Conn where

import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Swerve.API.Spec (ReqBody')
import Swerve.API.Verb (Verb)

type ConnectionRow cap qry hdr bdy
  = ( capture     :: Record cap
    , query       :: Record qry
    , header      :: Record hdr
    , body        :: bdy
    )
    
class Conn specs (conn :: # Type) | specs -> conn 

class HasConn (specs :: RowList) (conn :: # Type) | specs -> conn 

instance conn :: 
  ( HasConn spcl conn 
  , RL.RowToList specs spcl
  ) => Conn (Verb path status t specs) conn

instance hasConnNil :: HasConn RL.Nil conn
else instance hasConnCapture :: HasConn tail rest => HasConn (RL.Cons "capture" ctype tail) (capture :: ctype | rest)
else instance hasConnQuery   :: HasConn tail rest => HasConn (RL.Cons "query" qtype tail) (query :: qtype | rest)
else instance hasConnHeader  :: HasConn tail rest => HasConn (RL.Cons "header" htype tail) (header :: htype | rest)
else instance hasConnBody    :: HasConn tail rest => HasConn (RL.Cons "body" (ReqBody' btype ctype) tail) (body :: btype | rest)
else instance hasConn        :: HasConn tail rest => HasConn (RL.Cons k ctype tail) rest