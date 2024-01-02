module Config.Redis where

import           Data.Word

data Redis = Redis
  { getHost      :: !String,
    getPort      :: !Word16,
    getDSN       :: !String,
    isDSNEnabled :: !Bool
  }

dummy :: Redis
dummy = Redis "" 0 "" False

setHost :: Redis -> String -> Redis
setHost x v = x {getHost = v}

setPort :: Redis -> Word16 -> Redis
setPort x v = x {getPort = v}

setDSN :: Redis -> String -> Redis
setDSN x v = x {getDSN = v}

setDSNEnabled :: Redis -> Bool -> Redis
setDSNEnabled x v = x {isDSNEnabled = v}
