module Swerve.API.ReqBody where 

import Swerve.API.Types (ContentType')

data ReqBody (ct :: ContentType') (a :: Type)