module Config.Storage where

import           Data.Word (Word16)

data Storage = Storage { getDriver    :: !String
                       , getUser      :: !String
                       , getPassword  :: !String
                       , getDatabase  :: !String
                       , getSchema    :: !String
                       , getMode      :: !String
                       , getDSN       :: !String
                       , getHost      :: !String
                       , getPort      :: !Word16
                       , isDSNEnabled :: !Bool
                       , isEncrypted  :: !Bool
                       } deriving Show

dummy :: Storage
dummy = Storage "" "" "" "" "" "" "" "" 0 False False

setDriver :: Storage -> String -> Storage
setDriver x v = x {getDriver = v}

setUser :: Storage -> String -> Storage
setUser x v = x {getUser = v}

setPassword :: Storage -> String -> Storage
setPassword x v = x {getPassword = v}

setDatabase :: Storage -> String -> Storage
setDatabase x v = x {getDatabase = v}

setSchema :: Storage -> String -> Storage
setSchema x v = x {getSchema = v}

setMode :: Storage -> String -> Storage
setMode x v = x {getMode = v}

setDSN :: Storage -> String -> Storage
setDSN x v = x {getDSN = v}

setHost :: Storage -> String -> Storage
setHost x v = x {getHost = v}

setPort :: Storage -> Word16 -> Storage
setPort x v = x {getPort = v}

setDSNEnabled :: Storage -> Bool -> Storage
setDSNEnabled x v = x {isDSNEnabled = v}

setEncrypted :: Storage -> Bool -> Storage
setEncrypted x v = x {isEncrypted = v}
