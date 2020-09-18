module Swerve.Server.Internal.RouterI where

-- import Prelude

-- import Control.Alt (class Alt, (<|>))
-- import Control.Monad.Error.Class (class MonadThrow, throwError)
-- import Control.Monad.Except (ExceptT(..), runExceptT)
-- import Data.Either (either)
-- import Data.Newtype (unwrap)
-- import Data.Symbol (SProxy(..))
-- import Effect.Aff (Aff)
-- import Effect.Aff.Class (class MonadAff)
-- import Network.Wai (Request, Application)
-- import Prim.RowList (class RowToList)
-- import Swerve.API.Combinators (type (:<|>), (:<|>))
-- import Swerve.API.ContentTypes (NoContent)
-- import Swerve.API.Raw (Raw')
-- import Swerve.API.StatusCode (S204)
-- import Swerve.API.Verb (Verb)
-- import Swerve.Internal.Router (class Router, router)
-- import Swerve.Server.Internal.Handler (HandlerT)
-- import Swerve.Server.Internal.Response (class HasResponse, SwerveResponse, runHandler)
-- import Swerve.Server.Internal.ServerError (ServerError)
-- import Swerver.Server.Internal.Conn (class Conn)
-- import Type.Data.Row (RProxy(..))
-- import Type.Equality (class TypeEquals)
-- import Type.Proxy (Proxy(..))

-- class RouterI layout handler m | layout -> handler, handler -> layout  where
--   routerI :: Proxy layout -> handler -> Request -> ExceptT ServerError Aff (m SwerveResponse)

-- instance routerIAlt :: 
--   ( RouterI a handlera m 
--   , RouterI b handlerb m 
--   ) => RouterI (a :<|> b) (handlera :<|> handlerb ) m where 
--   routerI _ (handlera :<|> handlerb) req = routerI (Proxy :: _ a) handlera req <|> routerI (Proxy :: _ b) handlerb req

-- else instance routerINoContent :: 
--   ( Router (Verb method S204 path specs) path specs params 
--   , Conn (Verb method S204 path specs) params
--   , HasResponse (HandlerT (Verb method S204 path specs) m NoContent) params m
--   ) => RouterI (Verb method S204 path specs) (HandlerT (Verb method S204 path specs) m NoContent) m where 
--   routerI specP handler req = do 
--     params  <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
--     pure $ runHandler params handler req

-- else instance routerIImpl :: 
--   ( Router (Verb method status path specs) path specs params 
--   , RowToList specs spcrl 
--   , Conn (Verb method status path specs) params
--   , HasResponse (HandlerT (Verb method status path specs) m result) params m
--   ) => RouterI (Verb method status path specs) (HandlerT (Verb method status path specs) m result) m where 
--   routerI specP handler req = do 
--     params  <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
--     pure $ runHandler params handler req

-- instance routerIRaw :: 
--   ( Router (Raw' path specs) path specs params 
--   , Conn (Raw' path specs) params
--   , TypeEquals Application waiApplication
--   , HasResponse (HandlerT (Raw' path specs) m waiApplication) params m
--   ) => RouterI (Raw' path specs) (HandlerT (Raw' path specs) m waiApplication) m where 
--   routerI specP handler req  = do 
--     params  <- router specP (SProxy :: _ path) (RProxy :: _ specs) (_.url $ unwrap req) req 
--     pure $ runHandler params handler req