{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Button (Button(..)) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Text (Text)

data Button = Button {
      name :: Text
    , inst :: Text
    , button :: Int
    , x :: Int
    , y :: Int
    } deriving (Generic, Show)

instance ToJSON Button where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Button where
  parseJSON = withObject "Button" $ \v -> Button
      <$> v .: "name"
      <*> v .: "instance"
      <*> v .: "button"
      <*> v .: "x"
      <*> v .: "y"
