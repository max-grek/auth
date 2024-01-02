module Connector.Storage where

import           Config.Storage             (Storage (..))
import qualified Config.Storage             as Storage
import qualified Connector.Storage.Query    as Query
import           Data.ByteString.UTF8       (fromString)
import           Data.Int
import qualified Data.Text                  as T
import           Data.UUID                  (UUID)
import           Entity.Account             (Account (..))
import           Hasql.Connection           (Connection, ConnectionError)
import qualified Hasql.Connection           as Connection
import           Hasql.Session              (QueryError, Session)
import qualified Hasql.Session              as Session
import           Hasql.Statement
import           Hasql.Transaction          (Transaction, statement)
import qualified Hasql.Transaction.Sessions as Tx

initialise :: Storage -> IO (Either ConnectionError Connection)
initialise = Connection.acquire . settings
  where
    settings x
      | isDSNEnabled x = fromString . getDSN $ x
      | otherwise =
          Connection.settings
            (fromString $ getHost x)
            (getPort x)
            (fromString $ getUser x)
            (fromString $ Storage.getPassword x)
            (fromString $ getDatabase x)

insertAccount :: Account -> Session ()
insertAccount acc = Session.statement acc Query.insertAccount

getAllAccounts :: Session [Account]
getAllAccounts = Session.statement () Query.getAllAccount

getAccount :: String -> Session (Maybe Account)
getAccount x = Session.statement (T.pack x) Query.getAccount

insertAccountHistory :: UUID -> Account -> Session ()
insertAccountHistory uuid acc = Session.statement (uuid, acc) Query.insertAccountHistory

-- transaction :: Connection -> Transaction a -> IO a
-- transaction conn tx = session conn (Tx.transaction Tx.Serializable Tx.Write tx)

run :: Connection -> Session a -> IO a
run conn sess = Session.run sess conn >>= either (fail . show) return

-- doTx :: UUID -> Account -> String -> String -> String -> Transaction Int64
-- doTx uuid acc x y z = do
--   success <- statement (uuid, acc) Query.insertAccountHistory
--   if success == 1
--     then statement (uuid, T.pack x, T.pack y, T.pack z) Query.insertAccount
--     else return 0

-- doTx :: [a] -> [Statement a b] -> Transaction b
-- doTx [] _ = Transaction.condemn
-- doTx _ [] = Transaction.condemn
-- doTx (x : xs) (s : st) = do
--   undefined

-- doTx :: NonEmpty a -> NonEmpty (Statement a b) -> Transaction b
-- doTx (x :| xs) (s :| st) = do
