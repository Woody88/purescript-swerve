module Swerve.Server.Internal.ReqBody where

import Prelude

import Control.Monad.Except (ExceptT(..), throwError, withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff as Error
import Effect.Ref as Ref
import Network.Wai (Request(..))
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream as Stream
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Swerve.API.ContentTypes (class MimeUnrender, mimeUnrender)
import Swerve.API.Spec (ReqBody'(..))
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class ReqBody (r :: # Type) b | r -> b where
  reqBody :: RProxy r -> Request -> ExceptT String Aff b

instance reqBodyI ::
  ( RowToList r rl
  , ReqBodyRL rl b
  ) => ReqBody r b where
  reqBody _ = reqBodyRL (RLProxy :: _ rl)

class ReqBodyRL (rl :: RowList) b | rl -> b where
  reqBodyRL :: RLProxy rl  -> Request -> ExceptT String Aff b

instance reqBodyRLConsNil ::  ReqBodyRL RL.Nil (ReqBody' Unit ctype) where 
  reqBodyRL _ _ = pure $ ReqBody' unit 

instance reqBodyRLConsBody :: 
  ( MimeUnrender ctype a
  ) => ReqBodyRL (RL.Cons "body" (ReqBody' a ctype) tail) (ReqBody' a ctype) where
  reqBodyRL _ (Request { body: Nothing } ) = throwError "Request Body Required, should throw appropriate error status"
  reqBodyRL _ req@(Request { body: Just stream } ) = do 
    bodyStr <- withExceptT Error.message $ ExceptT $ Aff.try $ Aff.makeAff \done -> do
      bufs <- Ref.new []
      Stream.onData stream \buf ->
        void $ Ref.modify (_ <> [buf]) bufs
      Stream.onEnd stream do
        body <- Ref.read bufs >>= Buffer.concat >>= Buffer.toString UTF8
        done $ Right body
      pure Aff.nonCanceler
    case mimeUnrender (Proxy :: _ ctype) bodyStr of 
      Left e     -> throwError e 
      Right body -> pure $ ReqBody' body

else instance reqBodyRLConsOther ::
  ( ReqBodyRL tl bdy
  ) => ReqBodyRL (RL.Cons k v tl) bdy where
  reqBodyRL _ = reqBodyRL (RLProxy :: _ tl)
