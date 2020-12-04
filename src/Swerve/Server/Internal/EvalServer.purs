module Swerve.Server.Internal.EvalServer where 

import Swerve.Server.Internal.Delayed (Delayed)
import Unsafe.Coerce (unsafeCoerce)

class EvalServer api api' | api -> api' 

toHoistServer :: forall server hoistServer. EvalServer server hoistServer => server -> hoistServer
toHoistServer = unsafeCoerce

toHandler :: forall api api' env. EvalServer api api' => Delayed env api -> Delayed env api'
toHandler = unsafeCoerce