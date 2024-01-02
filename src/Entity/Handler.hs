{-# LANGUAGE OverloadedStrings #-}

module Entity.Body where

import           Data.Aeson

data JSONBody = Login Body | Signup Body

data Body = Body
  { getEmail    :: String,
    getPassword :: String
  }

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v -> Body
    <$> v .: "email"
    <*> v .: "password"
