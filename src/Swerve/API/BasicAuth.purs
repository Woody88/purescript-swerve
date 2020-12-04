module Swerve.API.BasicAuth where 

import Prelude 

data BasicAuth (realm :: Symbol) (userData :: Type)

newtype BasicAuthData = BasicAuthData 
  { username :: String
  , password :: String
  }

derive newtype instance ordBasicAuthData :: Ord BasicAuthData
derive newtype instance eqBasicAuthData :: Eq BasicAuthData
derive newtype instance showBasicAuthData :: Show BasicAuthData
