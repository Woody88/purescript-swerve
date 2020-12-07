module Swerve.Server.Internal.EvalServer where 

import Swerve.API.Types
import Swerve.Server.Internal.Delayed (Delayed)
import Unsafe.Coerce (unsafeCoerce)

class EvalServer (api :: Type) (api' :: Type) | api -> api' 

instance evalServerSub :: EvalServer a b

toHoistServer :: forall server hoistServer. EvalServer server hoistServer => server -> hoistServer
toHoistServer = unsafeCoerce

toHandler :: forall api api' env. EvalServer api api' => Delayed env api -> Delayed env api'
toHandler = unsafeCoerce