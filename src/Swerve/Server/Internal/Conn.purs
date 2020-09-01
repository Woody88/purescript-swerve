module Swerver.Server.Internal.Conn where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Prim.Symbol as Symbol
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Swerve.API.Spec (ReqBody', Resource')
import Swerve.API.Verb (Verb)
import Type.Data.RowList (RLProxy(..))

type ConnectionRow cap qry hdr bdy
  = ( capture     :: Record cap
    , query       :: Record qry
    , header      :: Record hdr
    , body        :: bdy
    )
    
class Conn specs (conn :: # Type) | specs -> conn 

class HasConn (specs :: RowList) (conn :: # Type) | specs -> conn 

class MkConn (base :: # Type) (sub :: # Type) (conn :: # Type) | base conn -> sub where
  mkConn :: {|base} -> {|conn}

instance mkConnImpl ::
  ( RowToList sub rl
  , MkConnRL base rl () conn
  ) => MkConn base sub conn where
  mkConn base =
    Builder.build (mkConnRL base (RLProxy :: _ rl)) {}

class MkConnRL (base :: # Type) (rl :: RowList) (from :: # Type) (to :: # Type) | rl -> from to where
  mkConnRL :: {|base} -> RLProxy rl -> Builder {|from} {|to}

instance mkConnRLNil :: MkConnRL base RL.Nil () () where
  mkConnRL _ _ = identity

-- | Ignore Resource when making connection 
instance mkConnRLConsResource :: MkConnRL base (RL.Cons "resource" (Resource' v ctype) tl) from from where
  mkConnRL b rl = identity

-- | Inject body that is within ReqBody' in connection 
else instance mkConnRLConsBody ::
  ( MkConnRL base tl from from'
  , Symbol.Append "body" "" k
  , IsSymbol k
  , Row.Cons k v _b base
  , Row.Cons k v from' to
  , Row.Lacks k from'
  ) => MkConnRL base (RL.Cons "body" (ReqBody' v ctype) tl) from to where
  mkConnRL base _ = hBuilder <<< tlBuilder
    where
      tlBuilder = mkConnRL base (RLProxy :: _ tl)
      sp = SProxy :: _ k
      v = Record.get sp base
      hBuilder = Builder.insert (SProxy :: _ k) v

-- Inject everything else in Connection
else instance mkConnRLCons ::
  ( MkConnRL base tl from from'
  , IsSymbol k
  , Row.Cons k v _b base
  , Row.Cons k v from' to
  , Row.Lacks k from'
  ) => MkConnRL base (RL.Cons k v tl) from to where
  mkConnRL base _ = hBuilder <<< tlBuilder
    where
      tlBuilder = mkConnRL base (RLProxy :: _ tl)
      sp = SProxy :: _ k
      v = Record.get sp base
      hBuilder = Builder.insert (SProxy :: _ k) v

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