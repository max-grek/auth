{-# LANGUAGE OverloadedStrings #-}

module Connector.Query (insertAccountHistory, insertAccount, getAllAccount) where

import           Contravariant.Extras.Contrazip
import           Data.ByteString
import           Data.Functor.Contravariant
import           Data.Int
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Time                      (LocalTime)
import           Data.UUID                      (UUID)
import           Entity.Account                 (Account (..))
import           Hasql.Decoders                 (Result)
import qualified Hasql.Decoders                 as D
import qualified Hasql.Encoders                 as E
import           Hasql.Statement                (Statement (..))

insertAccount :: Statement (UUID, Text, Text, Text) ()
insertAccount = Statement sql p D.noResult False
  where
    t1 = E.param (E.nonNullable E.uuid)
    t2 = E.param (E.nonNullable E.text)
    t3 = E.param (E.nonNullable E.text)
    t4 = E.param (E.nonNullable E.text)
    p = contrazip4 t1 t2 t3 t4
    sql = "INSERT INTO userdata.account (id, email, password, status) VALUES ($1, $2, $3, $4)"

getAllAccount :: Statement () [Account]
getAllAccount = Statement sql E.noParams decoder False
  where
    decoder =
      D.rowList $
        Account
          <$> D.column (D.nonNullable D.uuid)
          <*> D.column (D.nonNullable $ fmap T.unpack D.text)
          <*> D.column (D.nonNullable $ fmap T.unpack D.text)
          <*> D.column (D.nonNullable $ fmap T.unpack D.text)
          <*> D.column (D.nonNullable D.timestamp)
    sql = "SELECT id, email, password, status, created_at FROM userdata.account"

insertAccountHistory :: Statement (UUID, Account) ()
insertAccountHistory = Statement sql p D.noResult False
  where
    t1 = E.param (E.nonNullable E.uuid)
    t2 =
      (getId >$< E.param (E.nonNullable E.uuid))
        <> ((T.pack . getEmail) >$< E.param (E.nonNullable E.text))
        <> ((T.pack . getPassword) >$< E.param (E.nonNullable E.text))
        <> ((T.pack . getStatus) >$< E.param (E.nonNullable E.text))
        <> (getCreatedAt >$< E.param (E.nonNullable E.timestamp))
    p = contrazip2 t1 t2
    sql = "INSERT INTO userdata.account_history (id, account_id, email, password, status) VALUES ($1, $2, $3, $4, $5)"
