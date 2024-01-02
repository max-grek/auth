{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Handler where

import qualified Connector.Storage   as Connector
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Binary
import           Data.Binary.Builder
import           Data.Time           (UTCTime, utc, utcToLocalTime)
import           Data.UUID           (UUID, toByteString, toLazyASCIIBytes)
import           Entity.Account
import           Entity.Body
import           Hasql.Connection    (Connection)
import           Hasql.Session
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse

signup :: UUID -> UTCTime -> Request -> Connection -> IO Response
signup uuid t req conn = do
  body <- getRequestBodyChunk req
  case (decodeStrict body :: Maybe JSONBody) of
    Nothing -> badRequest "invalid body"
    Just v  ->
      let
        acc = Account
          { getId = uuid,
            getEmail = Entity.Body.getEmail v,
            getPassword = Entity.Body.getPassword v,
            getStatus = "1",
            getCreatedAt = utcToLocalTime utc t
            }
      in do
        v <- try (Connector.run conn (Connector.insertAccount acc)) :: IO (Either SomeException ())
        case v of
          Left e -> serverError
          Right _ -> pure $ responseBuilder status200 [] $ fromLazyByteString $ toLazyASCIIBytes uuid

badRequest :: String -> IO Response
badRequest str = pure $ responseBuilder status400 [] $ putStringUtf8 str

serverError :: IO Response
serverError = pure $ responseBuilder status500 [] ""

-- json :: ToJSON a => a -> IO Response
-- json x = pure $ responseBuilder status200 [(hContentType, "application/json")] . fromEncoding . toEncoding x

login :: Request -> Connection -> IO Response
login req conn = do
  body <- getRequestBodyChunk req
  case (decodeStrict body :: Maybe JSONBody) of
    Nothing -> badRequest "invalid body"
    Just v -> do
      let email = Entity.Body.getEmail v
      r <- try (Connector.run conn (Connector.getAccount email)) :: IO (Either SomeException (Maybe Account))
      case r of
        Left e -> serverError
        Right _ -> do
          pure $ responseBuilder status200 [] $ putStringUtf8 "yes"


