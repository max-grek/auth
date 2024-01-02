module Config.Http where

import           Data.Word (Word16)

data HTTP = HTTP { getHost :: String
                 , getPort :: Word16
                 } deriving Show

dummy :: HTTP
dummy = HTTP "" 0

setHost :: HTTP -> String -> HTTP
setHost x v = x {getHost = v}

setPort :: HTTP -> Word16 -> HTTP
setPort x v = x {getPort = v}
