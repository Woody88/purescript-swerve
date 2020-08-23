module Swerve.Internal.ParseSpec where

import Prelude

import Control.Monad.Except (ExceptT(..), except, throwError, withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, try)
import Effect.Aff as Aff
import Effect.Aff as Error
import Effect.Ref as Ref
import Network.Wai (Request(..))
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as HTTP
import Node.Stream as Stream
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Swerve.API.ContentTypes (class MimeUnrender, mimeUnrender)
import Swerve.API.Spec (ReqBody'(..))
import Swerve.Server.Internal.ParseHeader (class ParseHeader, parseHeader)
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

class ParseConnSpec   
  (specs :: RowList)
  (hfrom :: # Type) (hto :: # Type) 
  |specs -> hfrom hto where 
  parseConnSpec :: 
    RLProxy specs 
    -> Request 
    -> ExceptT String Aff { header :: Builder {| hfrom } {| hto } }

instance parseConnSpecNil :: ParseConnSpec RL.Nil hto hto where 
  parseConnSpec _ req = pure { header: identity }


instance parseConnSpecHeader ::
  ( RowToList htypes hrl
  , ParseHeader hrl hfrom' hto 
  , ParseConnSpec tail hfrom hfrom'
  ) => ParseConnSpec (RL.Cons "header" { | htypes } tail) hfrom hto where
  parseConnSpec _ req = do
    specs <- parseConnSpec (RLProxy :: _ tail) req 
    hdr <- except $ parseHeader (RLProxy :: _ hrl) (_.headers $ unwrap req) 
    pure $ { header: hdr <<< specs.header }

-- else instance parseConnSpecReqBody ::
--   ( MimeUnrender ctype a
--   , Row.Cons "req" a bfrom' bto 
--   , Row.Lacks "req" bfrom'
--   , ParseConnSpec tail hfrom hto bfrom bfrom'
--   ) => ParseConnSpec (RL.Cons "body" (ReqBody' a ctype) tail) hfrom hto bfrom (req :: a | bfrom') where
--   parseConnSpec _ (Request { body: Nothing } ) = throwError "Request Body Required, should throw appropriate error status"
--   parseConnSpec _ req@(Request { body: Just stream } ) = do 
--     bodyStr <- withExceptT Error.message $ ExceptT $ Aff.try $ Aff.makeAff \done -> do
--       bufs <- Ref.new []
--       Stream.onData stream \buf ->
--         void $ Ref.modify (_ <> [buf]) bufs
--       Stream.onEnd stream do
--         body <- Ref.read bufs >>= Buffer.concat >>= Buffer.toString UTF8
--         done $ Right body
--       pure Aff.nonCanceler

--     specs <- parseConnSpec (RLProxy :: _ tail) req 
--     case mimeUnrender (Proxy :: _ ctype) bodyStr of 
--       Left e     -> throwError e 
--       Right body -> do 
--         let v = Builder.insert (SProxy :: _ "req") body
--         pure $ { body: v <<< specs.body, header: specs.header }

else instance parseConnSpecs :: ParseConnSpec tail hfrom hto => ParseConnSpec (RL.Cons specs htype tail) hfrom hto where 
  parseConnSpec _ req = parseConnSpec (RLProxy :: _ tail) req