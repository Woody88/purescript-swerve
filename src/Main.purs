module Main where

import Prelude
import Swerve.Server.Internal.Handler (Handler)
import Control.Monad.Reader (asks)
import Effect (Effect)
import Swerve.API.Verb (Get)
import Swerve.Server (swerve)
import Type.Proxy (Proxy(..))

type  UserAPI = GetUser 

type GetUser 
    = Get "/hello/:id" (Capture { id :: Int })


type UpdateShipById
    = Get "/ship/:id/update?[imoNumber]&[name]" 
        ( Capture { id :: Int }
        )

type Server = UserAPI 

getUser :: Handler GetUser String  
getUser = do 
    num <- asks _.capture.id
    pure $ show num

userAPI =  getUser 

swerveTest = swerve (Proxy :: _ UserAPI) userAPI






type Capture captures = (capture :: captures)

main :: Effect Unit
main = pure unit -- Warp.run 8000 app

-- type Handler conn a = ReaderT conn (ExceptT String Effect) a  