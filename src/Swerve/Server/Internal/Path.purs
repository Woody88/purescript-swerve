  
module Swerve.Server.Internal.Path where

import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)
import Type.Data.Symbol (SProxy)

foreign import kind Path 
foreign import data Var :: Symbol -> Path 
foreign import data Segment :: Symbol -> Path

foreign import kind PList
foreign import data PNil :: PList
foreign import data PCons :: Path -> PList -> PList

data PProxy (pl :: PList) = PProxy

class Parse (string :: Symbol) (format :: PList) | string -> format

instance aParse :: Fail (Text "Invalid path. Empty is not a valid path!") => Parse "" wrong
else instance  bParse :: ParseImpl str pl => Parse str pl

class ParseImpl (string :: Symbol) (format :: PList) | string -> format

instance aParseImpl :: ParseImpl "" PNil -- Nil
else instance bParseImpl :: ParseImpl "/" (PCons (Segment "") PNil) -- Root path 
else instance cParseImpl :: (Symbol.Cons h t str, ParseSegment h t pl) => ParseImpl str pl -- Start Parsing 

class ParseSegment (head :: Symbol) (tail :: Symbol) (out :: PList) | head tail -> out

-- Parse Segment 
-- Call variable matcher when ':' found. 
-- Throw error message on trailing '/'
instance aParseSegment :: ParseSegment "" a PNil
else instance bParseSegment :: ParseSegment a "" (PCons (Segment a) PNil)  
else instance cParseSegment :: Fail (Text "Invalid path. Remove trailing '/' from your path!") => ParseSegment a "/" o
else instance dParseSegment :: 
  ( Symbol.Cons h t str
  , ParseVar h t (Var var) rest
  , ParseImpl rest prest
  ) => ParseSegment ":" str (PCons (Var var) prest)  
else instance eParseSegment :: (ParseImpl str o) => ParseSegment "/" str (PCons (Segment "") o)
else instance fParseSegment ::
  ( ParseImpl s (PCons (Segment acc) r)
  , Symbol.Cons o acc rest
  ) => ParseSegment o s (PCons (Segment rest) r)

class ParseVar (h :: Symbol) (t :: Symbol) (var :: Path) (rest :: Symbol)| h t -> var rest

-- Parse variable 
-- Stop parsing when "" or "/" found.
-- Throw error message on trailing '/'
instance aParseVar :: ParseVar "" a (Var "") ""
else instance bParseVar :: ParseVar a "" (Var a) ""
else instance cParseVar :: Fail (Text "Invalid path. Remove trailing '/' from your path!") => ParseVar a "/" (Var "yo") ""
else instance dParseVar :: ParseVar "/" i (Var "") i
else instance eParseVar ::
  ( Symbol.Cons h' t' t
  , ParseVar h' t' (Var var) rest
  , Symbol.Cons h var var'
  ) => ParseVar h t (Var var') rest

parse :: forall i o. Parse i o => SProxy i ->  PProxy o
parse _ = PProxy :: _ o  