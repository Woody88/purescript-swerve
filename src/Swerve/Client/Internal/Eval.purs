module Swerve.Client.Internal.Eval where 

import Data.Symbol
import Data.Variant (Variant)
import Swerve.API.Alternative (type (:<|>))
import Swerve.API.Sub 
import Swerve.API.Verb (Verb)
import Unsafe.Coerce (unsafeCoerce)

data Client (m :: Type -> Type) (api :: Type)

class EvalClient (a :: Type) (b :: Type) | a -> b

instance _evalClientAlt :: EvalClient (Client m (a :<|> b) ) ((Client m a) :<|> (Client m b))

instance _evalClientVerb :: EvalClient (Client m (Verb method cts as)) (m (Variant as))

instance _evalClientPath 
  :: IsSymbol path 
  => EvalClient (Client m (path :> api)) (Client m api)

class EvalHandler (a :: Type) (b :: Type) | a -> b

instance evalHandlerPath :: 
  ( IsSymbol path 
  , EvalHandler api server
  ) => EvalHandler (path :> api) server

instance evalHandlerSVerb :: EvalHandler (Verb method ctypes as) (m (Variant as))

eval :: forall a b. EvalClient a b => a -> b
eval = unsafeCoerce

lift :: forall a b. EvalClient b a => a -> b
lift = unsafeCoerce