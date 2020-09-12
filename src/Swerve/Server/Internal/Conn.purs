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
import Swerve.API.Raw (Raw')
import Swerve.API.Spec (ReqBody', Resource')
import Swerve.API.Verb (Verb)
import Type.Data.RowList (RLProxy(..))
import Type.Equality (class TypeEquals)
import Type.RowList (class ListToRow)

type ConnectionRow cap qry hdr bdy
  = ( capture     :: Record cap
    , query       :: Record qry
    , header      :: Record hdr
    , body        :: bdy
    )

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

class Conn specs (conn :: # Type) | specs -> conn 

class HasConn (specs :: RowList) (conn :: RowList) | specs ->  conn

-- | creates a connection row type of based on user specs
instance connRaw :: TypeEquals (Record ()) { | row } => Conn (Raw' path row) row

instance conn :: 
  ( RowToList specs spcl
  , HasConn spcl connrl
  , ListToRow connrl conn
  ) => Conn (Verb path status t specs) conn

instance hasConnNil :: HasConn RL.Nil RL.Nil

instance hasConnCapture :: HasConn tail connrl => HasConn (RL.Cons "capture" t tail) (RL.Cons "capture" t connrl)
else instance hasConnQuery :: HasConn tail connrl => HasConn (RL.Cons "query" t tail) (RL.Cons "query" t connrl)
else instance hasConnHeader :: HasConn tail connrl => HasConn (RL.Cons "header" t tail) (RL.Cons "header" t connrl)
else instance hasConnBody :: HasConn tail connrl => HasConn (RL.Cons "body" (ReqBody' t ctype) tail) (RL.Cons "body" t connrl)
else instance hasConn :: HasConn tail connrl => HasConn (RL.Cons k t tail) connrl