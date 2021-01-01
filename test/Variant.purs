module Test.Variant where 

import Data.Variant
import Type.Row as R
import Type.RowList as RL
import  Type.Proxy 

class Change (rl :: RL.RowList Type) (rl' :: RL.RowList Type) b | rl b -> rl'


instance changeMatchNil
  ∷ Change RL.Nil RL.Nil b

instance changeMatchCons
  ∷ ( Change rl rl' b
    )
  => Change (RL.Cons sym a rl) (RL.Cons sym b rl') b


change :: forall r r' rl rl' b. RL.RowToList r rl => RL.ListToRow rl' r' => Change rl rl' b =>  Proxy r -> Proxy b -> Proxy r'
change _ _= Proxy

row = Proxy :: _  (a :: String, b :: String) 
int = Proxy :: _ Int 

-- forall t96 t98. ListToRow t98 t96 => Change (Cons "a" String (Cons "b" String Nil)) t98 Int => Proxy t96


x = change row int 