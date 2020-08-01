module Swerve.Server.Internal.ParseCapture where

-- Credits to @justinwoo for this module

-- import Prelude
-- import Data.Either (Either(..))
-- import Data.Int as Int
-- import Data.Maybe (Maybe(..))
-- import Data.String (Pattern(..))
-- import Data.String (indexOf, splitAt, stripPrefix) as String
-- import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
-- import Global as Global
-- import Prim.Row as Row
-- import Prim.Symbol as Symbol
-- import Prim.TypeError as TE
-- import Record.Builder (Builder)
-- import Record.Builder as Builder
-- import Record.Format (class Parse, FCons, FNil, FProxy(..), Lit, Var, kind FList)


-- class ParseCapture (url :: Symbol) (captures :: # Type) where
--     parseCapture :: SProxy url ->  String -> Either String (Record captures)

-- instance parseCaptures :: 
--     ( Parse url row 
--     , ParseCaptureImpl row () captures
--     ) => ParseCapture  url captures where 
--     parseCapture _ url = flip Builder.build {} <$> parseCaptureImpl (FProxy :: FProxy row) url

-- class ParseCaptureImpl (row :: FList) (from :: # Type) (to :: # Type)
--     | row -> from to where 
--     parseCaptureImpl :: FProxy row -> String -> Either String (Builder (Record from) (Record to))

-- instance parseCaptureImplNil ::  ParseCaptureImpl FNil () () where
--     parseCaptureImpl _ _ = pure identity

-- instance parseCaptureImplCons ::  
--     ( ParseTypedCapture s name ty 
--     , ReadCapture ty 
--     , IsSymbol name 
--     , Row.Cons name ty from' to 
--     , Row.Lacks name from'
--     , ParseCaptureImpl tail from from'
--     ) => ParseCaptureImpl (FCons (Var s) tail) from to where 
--     parseCaptureImpl _ url = do 
--         value <- readCapture split.before 
--         let first = Builder.insert nameP value  
--         rest <-  parseCaptureImpl (FProxy :: FProxy tail) split.after 
--         pure $ first <<< rest
--         where 
--             nameP = SProxy :: SProxy name 
--             name = reflectSymbol nameP  
--             split = case String.indexOf (Pattern "/") url of 
--                 Just index -> String.splitAt index url  
--                 Nothing    -> { before: url, after: mempty } 

-- instance parseCaptureImplConsLit ::
--   ( IsSymbol segment
--   , ParseCaptureImpl tail from to
--   ) => ParseCaptureImpl (FCons (Lit segment) tail) from to where
--     parseCaptureImpl _ url =
--         case String.stripPrefix (Pattern segment) url of
--             Nothing ->
--                 Left $ "Could not strip segment " <> segment <> " from path " <> url
--             Just remaining ->
--                 parseCaptureImpl (FProxy :: FProxy tail) remaining
--             where
--             segment = reflectSymbol (SProxy :: SProxy segment)

-- class ParseTypedCapture (s :: Symbol) (name :: Symbol) (ty :: Type) | s -> name ty

-- instance parseTypedCapture ::
--   ( Symbol.Cons x xs s
--   , ParseTypedCaptureImpl x xs "" name ty
--   ) => ParseTypedCapture s name ty

-- class ParseTypedCaptureImpl
--   (x :: Symbol) (xs :: Symbol) (acc :: Symbol)
--   (name :: Symbol) (ty :: Type)
--   | x xs acc -> name ty

-- instance parseTypedCaptureImplNoMatch ::
--   ( Symbol.Append acc x name
--   ) => ParseTypedCaptureImpl x "" acc name String

-- else instance parseTypedCaptureImplColonSplit ::
--   ( MatchTypeName tyName ty
--   ) => ParseTypedCaptureImpl ":" tyName name name ty

-- else instance parseTypedCaptureImplBase ::
--   ( Symbol.Cons y ys xs
--   , Symbol.Append acc x acc'
--   , ParseTypedCaptureImpl y ys acc' name ty
--   ) => ParseTypedCaptureImpl x xs acc name ty

-- class MatchTypeName (s :: Symbol) (ty :: Type) | s -> ty
-- instance matchTypeNameString :: MatchTypeName "String" String
-- else instance matchTypeNameInt :: MatchTypeName "Int" Int
-- else instance matchTypeNameErr ::
--   ( Symbol.Append "Can't match type annotation to type: " s msg
--   , TE.Fail (TE.Text msg)
--   ) => MatchTypeName s ty

-- -- convert capture strings
-- class ReadCapture a where
--   readCapture :: String -> Either String a

-- instance readCaptureString :: ReadCapture String where
--   readCapture s = pure s

-- instance readCaptureInt :: ReadCapture Int where
--   readCapture s =
--     case Int.fromNumber $ Global.readInt 10 s of
--       Just a -> pure a
--       Nothing ->
--         Left $ "could not parse " <> s <> " into integer"