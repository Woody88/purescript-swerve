module Swerve.Client.Internal.ClientError where 

import Prelude 

data ClientError = ConnectionError String 

instance instShowClientError :: Show ClientError where 
  show (ConnectionError s) = s 