{-# LANGUAGE OverloadedStrings #-}

module Entity.Body where

import           Data.Aeson

data Body = Login JSONBody | Signup JSONBody

data JSONBody = JSONBody
  { getEmail    :: String,
    getPassword :: String
  }

instance FromJSON JSONBody where
  parseJSON = withObject "JSONBody" $ \v -> JSONBody
    <$> v .: "email"
    <*> v .: "password"
