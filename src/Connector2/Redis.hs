module Connector.Redis where

import           Config.Redis
import qualified Config.Redis   as Config (Redis (..))
import           Data.UUID      (UUID)
import           Database.Redis (ConnectInfo, Connection, PortID (..))
import qualified Database.Redis as Redis
import           Entity.Token

initialise :: Config.Redis -> IO Connection
initialise = Redis.checkedConnect . settings
  where
    settings :: Config.Redis -> ConnectInfo
    settings x
      | isDSNEnabled x = case Redis.parseConnectInfo . getDSN $ x of
          Left e  -> Redis.defaultConnectInfo
          Right v -> v
      | otherwise =
          Redis.defaultConnectInfo
            { Redis.connectHost = Config.getHost x,
              Redis.connectPort = PortNumber . fromIntegral $ Config.getPort x
            }

makeToken :: UUID -> Maybe Token
makeToken x = undefined
