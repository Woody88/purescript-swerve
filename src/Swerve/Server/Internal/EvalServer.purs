module Swerve.Server.Internal.EvalServer where 

import Effect.Aff (Aff)
import Swerve.API.Types (type (:<|>), type (:>))
import Swerve.Server.Internal.Delayed (Delayed)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

class EvalServer m (api :: Type) (api' :: Type) | m api -> api' 


instance evalServerSeg  ::  EvalServer s (s (a :> api) m) (s api m)
else instance evalServerY  ::  EvalServer s (s a m) b

toHoistServer :: forall m server hoistServer. EvalServer m server hoistServer => Proxy m -> server -> hoistServer
toHoistServer _ = unsafeCoerce

toHandler :: forall m api api' env. EvalServer m api api' => Proxy m -> Delayed env api -> Delayed env api'
toHandler _ = unsafeCoerce