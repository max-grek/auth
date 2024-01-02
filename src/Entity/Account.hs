{-# LANGUAGE OverloadedStrings #-}

module Entity.Account where

--import           Data.Aeson
import           Data.Time (LocalTime)
import           Data.UUID (UUID)

data Account = Account
  { getId        :: UUID,
    getEmail     :: String,
    getPassword  :: String,
    getStatus    :: String,
    getCreatedAt :: LocalTime
  }
  deriving (Show)

-- instance ToJSON Account where
--   toJSON (Account id email pwd status cat) =
--     object
--       [ "uuid" .= id,
--         "email" .= email,
--         "password" .= pwd,
--         "status" .= status,
--         "created_at" .= cat
--       ]

--   toEncoding (Account id email pwd status cat) =
--     pairs ("uuid" .= id <> "email" .= email <> "password" .= pwd <> "status" .= status <> "created_at" .= cat)

-- instance FromJSON Account

setId :: Account -> UUID -> Account
setId x v = x {getId = v}

setEmail :: Account -> String -> Account
setEmail x v = x {getEmail = v}

setPassword :: Account -> String -> Account
setPassword x v = x {getPassword = v}

setStatus :: Account -> String -> Account
setStatus x v = x {getStatus = v}

setCreatedAt :: Account -> LocalTime -> Account
setCreatedAt x v = x {getCreatedAt = v}
