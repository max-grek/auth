module Entity.Token where

import           Data.Int
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Time.Clock    (UTCTime)
import           Data.UUID

data Token =
  Token
    { getAccessToken  :: !String
    , getRefreshToken :: !String
    , getAccessUUID   :: !String
    , getRefreshUUID  :: !String
    , getATExpires    :: !Int64
    , getRTExpires    :: !Int64
    }

data Claim = Claim { userId    :: UUID
                   , userName  :: String
                   , isRoot    :: Bool
                   , createdAt :: UTCTime
                   , accounts  :: NonEmpty UUID}
