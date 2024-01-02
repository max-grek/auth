module JWT where

import           Data.UUID
import qualified Libjwt.Payload as P
--import           Web.Libjwt

data UserClaims = UserClaims { user_id :: Int
                             , at_id   :: Int
                             }
-- mkPayload current =
--   let now = fromUTC current
--   in def
--      { iss = Iss (Just "myApp")
--      , aud = Aud ["https://myApp.com"]
--      , iat = Iat (Just now)
--      , P.exp = Exp (Just $ now plusSeconds 300)
--      , privateClaims = toPrivateClaims
--                        UserClaims { userName = "huy"
--                                   , isRoot = False
--                                   , userID = 12312
--                                   }
--      }

-- mkClaims =
--   ClaimsSet { iss = Iss (Just "huy")
--             , exp = Exp (Just $ now plusSeconds 300)
--             ,
--             }
