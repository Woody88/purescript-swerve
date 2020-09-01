module Swerve.Server.Internal.ParseBody where

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
import Swerve.API.ContentTypes (class MimeUnrender, PlainText, mimeUnrender)
import Swerve.API.Spec (Header'(..), ReqBody'(..), Resource'(..))
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class ParseBody (r :: # Type) b | r -> b where
  parseBody :: RProxy r -> Request -> ExceptT String Aff b

instance parseBodyI ::
  ( RowToList r rl
  , ParseBodyRL rl b
  ) => ParseBody r b where
  parseBody _ = parseBodyRL (RLProxy :: _ rl)

class ParseBodyRL (rl :: RowList) b | rl -> b where
  parseBodyRL :: RLProxy rl  -> Request -> ExceptT String Aff b

class ParseBodyCT (rl :: RowList) ctype  | rl -> ctype 

instance parseBodyCT :: ParseBodyCT (RL.Cons "body" (ReqBody' a ctype) tail) ctype

class ParseResource (rl :: RowList) a ctype  | rl -> a ctype 

-- add a parse resource compiler error when RL.Nil 
-- it means that the user did not add resource in the specs row
instance parseResourceHeader :: ParseResource (RL.Cons "resource" (Resource' (Header' r a) ctype) tail) a ctype
else instance parseResource :: ParseResource (RL.Cons "resource" (Resource' a ctype) tail) a ctype
else instance parseResource' ::
  ( ParseResource tl a ctype
  ) => ParseResource (RL.Cons k v tl) a ctype

instance parseBodyRLConsNil ::  ParseBodyRL RL.Nil (ReqBody' Unit ctype) where 
  parseBodyRL _ _ = pure $ ReqBody' unit 

instance parseBodyRLConsBody :: 
  ( MimeUnrender ctype a
  ) => ParseBodyRL (RL.Cons "body" (ReqBody' a ctype) tail) (ReqBody' a ctype) where
  parseBodyRL _ (Request { body: Nothing } ) = throwError "Request Body Required, should throw appropriate error status"
  parseBodyRL _ req@(Request { body: Just stream } ) = do 
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

else instance parseBodyRLConsOther ::
  ( ParseBodyRL tl bdy
  ) => ParseBodyRL (RL.Cons k v tl) bdy where
  parseBodyRL _ = parseBodyRL (RLProxy :: _ tl)
