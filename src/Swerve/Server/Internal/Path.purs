  
module Swerve.Server.Internal.Path where

import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Text)
import Type.Data.Symbol (SProxy)

foreign import kind Path 
foreign import data CaptureVar :: Symbol -> Path 
foreign import data QueryVar :: Symbol -> Path 
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
else instance bParseImpl :: ParseImpl "[" PNil
else instance cParseImpl :: ParseImpl "/" (PCons (Segment "") PNil) -- Root path 
else instance dParseImpl :: (Symbol.Cons h t str, ParseSegment h t pl) => ParseImpl str pl -- Start Parsing 

class ParseSegment (head :: Symbol) (tail :: Symbol) (out :: PList) | head tail -> out

-- Parse Segment 
-- Call variable matcher when ':' found. 
-- Throw error message on trailing '/'
instance aParseSegment :: ParseSegment "" a PNil
else instance bParseSegment :: ParseSegment a "" (PCons (Segment a) PNil)  
else instance cParseSegment :: Fail (Text "Invalid path. Remove trailing '/' from your path!") => ParseSegment a "/" o
else instance dParseSegment :: 
  ( Symbol.Cons h t str
  , ParseImpl str prest
  ) => ParseSegment "&" str prest
else instance eParseSegment :: 
  ( Symbol.Cons h t str
  , ParseQueryVar h t (QueryVar var) rest
  , ParseImpl rest prest
  ) => ParseSegment "[" str (PCons (QueryVar var) prest)  
else instance gParseSegment :: 
  ( Symbol.Cons h t str
  , ParseCaptureVar h t (CaptureVar var) rest
  , ParseImpl rest prest
  ) => ParseSegment ":" str (PCons (CaptureVar var) prest)  
else instance hParseSegment :: (ParseImpl str o) => ParseSegment "?" str (PCons (Segment "") o)
else instance iParseSegment :: (ParseImpl str o) => ParseSegment "/" str (PCons (Segment "") o)
else instance jParseSegment ::
  ( ParseImpl s (PCons (Segment acc) r)
  , Symbol.Cons o acc rest
  ) => ParseSegment o s (PCons (Segment rest) r)

class ParseCaptureVar (h :: Symbol) (t :: Symbol) (var :: Path) (rest :: Symbol)| h t -> var rest

-- Parse variable 
-- Stop parsing when "" or "/" found.
-- Throw error message on trailing '/'
instance aParseCaptureVar :: ParseCaptureVar "" a (CaptureVar "") ""
else instance bParseCaptureVar :: ParseCaptureVar a "" (CaptureVar a) ""
else instance dParseCaptureVar :: Fail (Text "Invalid path. Remove trailing '/' from your path!") => ParseCaptureVar a "/" (CaptureVar "") ""
else instance eParseCaptureVar :: ParseCaptureVar "/" i (CaptureVar "") i
else instance fParseCaptureVar :: ParseCaptureVar "?" i (CaptureVar "") i
else instance gParseCaptureVar ::
  ( Symbol.Cons h' t' t
  , ParseCaptureVar h' t' (CaptureVar var) rest
  , Symbol.Cons h var var'
  ) => ParseCaptureVar h t (CaptureVar var') rest

class ParseQueryVar (h :: Symbol) (t :: Symbol) (var :: Path) (rest :: Symbol)| h t -> var rest

instance aParseQueryVar :: Fail (Text "Invalid path. Remove trailing '/' from your path!") => ParseQueryVar a "/" (QueryVar "") ""
-- else instance aParseQueryVar :: ParseQueryVar "" a (QueryVar "") ""
else instance bParseQueryVar :: ParseQueryVar "]" a (QueryVar "") a
else instance cParseQueryVar :: ParseQueryVar a "" (QueryVar a) ""
else instance dParseQueryVar ::
  ( Symbol.Cons h' t' t
  , ParseQueryVar h' t' (QueryVar var) rest
  , Symbol.Cons h var var'
  ) => ParseQueryVar h t (QueryVar var') rest

parse :: forall i o. Parse i o => SProxy i ->  PProxy o
parse _ = PProxy :: _ o  