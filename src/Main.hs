{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import           Config             (Config (..), HTTP, Storage, update,
                                     validate)
import qualified Config.Http        as Http hiding (dummy)
import qualified Config.Redis       as Redis hiding (dummy)
import qualified Config.Storage     as Storage hiding (dummy)
import           Flag               (define, parse)
import           Prelude
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

flags =
  [ define "http-host" "0.0.0.0" "http host",
    define "http-port" 9000 "http port",
    define "http-timeout" "15s" "http timeout",
    -- storage flags
    define "db-driver" "postgres" "database driver",
    define "db-user" "amc_dev" "database user",
    define "db-password" "amcdev" "database password",
    define "db-name" "amcdb_dev" "database name",
    define "db-schema" "userdata" "database schema",
    define "db-mode" "disable" "database mode",
    define "dsn" "postgresql://amc_dev:amcdev@0.0.0.0:5433/amcdb_dev?sslmode=disable" "database dsn",
    define "db-host" "0.0.0.0" "database host",
    define "db-port" 5433 "database port",
    define "is-dsn-enabled" False "switch between dsn and ...",
    define "is-db-encrypted" False "database data encryption",
    -- redis flags
    define "redis-host" "0.0.0.0" "redis host",
    define "redis-port" 0 "redis port"
  ]

updateHTTP :: HTTP -> (String, String) -> HTTP
updateHTTP x ("http-host", host) = Http.setHost x $ read host
updateHTTP x ("http-port", port) = Http.setPort x $ read port
updateHTTP x _                   = x

updateStorage :: Storage -> (String, String) -> Storage
updateStorage x ("db-driver", driver) = Storage.setDriver x $ read driver
updateStorage x ("db-user", user) = Storage.setUser x $ read user
updateStorage x ("db-password", password) = Storage.setPassword x $ read password
updateStorage x ("db-name", name) = Storage.setDatabase x $ read name
updateStorage x ("db-schema", schema) = Storage.setSchema x $ read schema
updateStorage x ("db-mode", mode) = Storage.setMode x $ read mode
updateStorage x ("dsn", dsn) = Storage.setDSN x $ read dsn
updateStorage x ("db-host", host) = Storage.setHost x $ read host
updateStorage x ("db-port", port) = Storage.setPort x $ read port
updateStorage x ("is-dsn-enabled", isDsn) = Storage.setDSNEnabled x $ read isDsn
updateStorage x ("is-encrypted", isEncrypt) = Storage.setEncrypted x $ read isEncrypt
updateStorage x _ = x

updateRedis :: Redis -> (String, String) -> Redis
updateRedis x ("redis-host", host) = Redis.setHost x $ read host
updateRedis x ("redis-port", port) = Redis.setPort x $ read port
updateRedis x _                    = x

main :: IO ()
main = do
  args <- getArgs
  case parse flags args of
    Left e -> do
      print e
      exitFailure
    Right v -> do
      let cfg = Config (update updateHTTP v) (update updateStorage v)
      print cfg
      print $ validate cfg
