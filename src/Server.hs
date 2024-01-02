{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Config.Redis
import           Config.Storage
import qualified Connector.Redis          as CR
import qualified Connector.Storage        as CS
import           Network.Handler.Auth     (login, signup)

import qualified Database.Redis           as Redis (Connection)
import qualified Hasql.Connection         as Hasql (Connection)

import           Data.Time                (getCurrentTime)
import           Data.UUID.V4             (nextRandom)
import           Network.HTTP.Types       (Method, methodPost, status400,
                                           status405)
import           Network.Wai              (Response, pathInfo, requestMethod,
                                           responseBuilder)
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  redisConn <- CR.initialise test2
  print "successfully established connection with redis"

  pgConn <- CS.initialise test
  either
    (\_ -> putStrLn "huy")
    (\pgConn -> do
        putStrLn "successfully established connection with storage"
        runApp redisConn pgConn
    ) pgConn

runApp :: Redis.Connection -> Hasql.Connection -> IO ()
runApp redisConn pgConn = do
  uuid <- nextRandom
  t <- getCurrentTime
  Warp.run 9000 $ \req send -> do
    response <-
      case pathInfo req of
        ["api", "signup"] ->
          case requestMethod req of
            "POST" -> signup uuid t req pgConn
            _      -> pure Main.badRequest
        ["api", "login"] ->
          case requestMethod req of
            "POST" -> login req pgConn
            _      -> pure Main.badRequest
        _ -> pure notFound
    send response

badRequest :: Response
badRequest = responseBuilder status405 [] "Bad request"

notFound :: Response
notFound = responseBuilder status400 [] "Not found"

test =
  Storage
    { getDriver = "postgres",
      getUser = "amc_dev",
      getPassword = "amcdev",
      getDatabase = "amcdb_dev",
      getSchema = "userdata",
      getMode = "disable",
      getDSN = "",
      getHost = "0.0.0.0",
      getPort = 5433,
      isDSNEnabled = False,
      isEncrypted = False
    }

test2 =
  Redis { getHost = "0.0.0.0"
        , getPort =  6379
        , getDSN = ""
        , isDSNEnabled = False
        }
