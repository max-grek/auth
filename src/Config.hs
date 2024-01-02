{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Config
  ( module Config.Http,
    module Config.Storage,
    module Config.Redis (setHost, setPort, setDSN, setDSNEnabled),
    Config (..),
    validate,
    update,
  )
where

import           Config.Http    (HTTP)
import qualified Config.Http    as Http
import           Config.Redis   (Redis)
import qualified Config.Redis   as Redis
import           Config.Storage (Storage (..))
import qualified Config.Storage as Storage
import           Data.Either    (isLeft)
import           Data.List      (lookup)
import           Data.Maybe     (fromMaybe)
import           Data.Word      (Word16)
import           Prelude

class Validate v where
  validate :: v -> [Either String ()]

data Config = Config HTTP Storage deriving (Show)

instance Validate HTTP where
  validate :: HTTP -> [Either String ()]
  validate x = filter isLeft $ zipWith _thenLeft cases errs
    where
      cases = [null $ Http.getHost x]
      errs = ["http host must not be empty"]

instance Validate Storage where
  validate :: Storage -> [Either String ()]
  validate x
    | isDSNEnabled x && (not . null $ getDSN x) = []
    | otherwise = filter isLeft $ zipWith _thenLeft cases errs
    where
      cases = fmap null funcs
      funcs =
        [ getDriver x,
          getUser x,
          getPassword x,
          getDatabase x,
          getSchema x,
          getMode x,
          Storage.getHost x
        ]

      errs = fmap (++ " must not be empty") fields
      fields =
        [ "driver",
          "user",
          "password",
          "database",
          "schema",
          "mode",
          "host"
        ]

instance Validate Redis where
  validate :: Redis -> [Either String ()]
  validate x
    | Redis.isDSNEnabled x && (not . null $ Redis.getDSN x) = []
    | otherwise = filter isLeft $ zipWith _thenLeft cases errs
    where
      cases = [null $ Redis.getHost x]
      errs = ["host must not be empty"]

instance Validate Config where
  validate :: Config -> [Either String ()]
  validate (Config http storage) = validate http <> validate storage

class Update v where
  update :: (Foldable t) => (v -> a -> v) -> t a -> v

instance Update HTTP where
  update f = foldl f Http.dummy

instance Update Storage where
  update f = foldl f Storage.dummy

instance Update Redis where
  update f = foldl f Redis.dummy

_thenLeft :: Bool -> a -> Either a ()
_thenLeft x y
  | x = Left y
  | otherwise = Right ()
