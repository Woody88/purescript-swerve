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
import Type.Data.Row (RProxy)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class ParseBody (r :: # Type) b ctype | r -> b ctype where
  parseBody :: RProxy r -> Request -> ExceptT String Aff b

instance parseBodyI ::
  ( RowToList r rl
  , ParseBodyRL rl b ctype
  ) => ParseBody r b ctype where
  parseBody _ = parseBodyRL (RLProxy :: _ rl)

class ParseBodyRL (rl :: RowList) b ctype | rl -> b ctype where
  parseBodyRL :: RLProxy rl  -> Request -> ExceptT String Aff b

class ParseBodyCT (rl :: RowList) ctype  | rl -> ctype 

instance parseBodyCT :: ParseBodyCT (RL.Cons "contentType" (Proxy ctype) tail) ctype
else instance parseBodyCT' ::
  ( ParseBodyCT tl ctype
  ) => ParseBodyCT (RL.Cons k v tl) ctype

instance parseBodyRLConsNil ::  ParseBodyRL RL.Nil Unit PlainText where 
  parseBodyRL _ _ = pure unit 

instance parseBodyRLConsBody :: 
  ( MimeUnrender ctype a
  , ParseBodyCT tail ctype
  ) => ParseBodyRL (RL.Cons "body" a tail) a ctype where
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
      Right body -> pure body

else instance parseBodyRLConsOther ::
  ( ParseBodyRL tl bdy ctype
  ) => ParseBodyRL (RL.Cons k v tl) bdy ctype where
  parseBodyRL _ = parseBodyRL (RLProxy :: _ tl)
