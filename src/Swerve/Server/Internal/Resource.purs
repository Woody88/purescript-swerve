module Swerve.Server.Internal.Resource where

-- import Prim.RowList (kind RowList)
-- import Prim.RowList as RL
-- import Swerve.API.Spec (Header', Resource')

-- class Resource (rl :: RowList) a ctype  | rl -> a ctype 

-- -- add a parse resource compiler error when RL.Nil 
-- -- it means that the user did not add resource in the specs row
-- instance resourceHeader :: Resource (RL.Cons "resource" (Resource' (Header' r a) ctype) tail) a ctype
-- else instance resource :: Resource (RL.Cons "resource" (Resource' a ctype) tail) a ctype
-- else instance resource' ::
--   ( Resource tl a ctype
--   ) => Resource (RL.Cons k v tl) a ctype