{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Click (Button(..), Click(..), buttonFromId) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Text (Text)

data Button = LeftClick
            | MiddleClick
            | RightClick
            | ScrollUp
            | ScrollDown
            | ScrollLeft
            | ScrollRight
            | Back
            | Forward
            | None

buttonFromId :: Int -> Maybe Button
buttonFromId 1 = Just LeftClick
buttonFromId 2 = Just MiddleClick
buttonFromId 3 = Just RightClick
buttonFromId 4 = Just ScrollUp
buttonFromId 5 = Just ScrollDown
buttonFromId 6 = Just ScrollLeft
buttonFromId 7 = Just ScrollRight
buttonFromId 8 = Just Back
buttonFromId 9 = Just Forward
buttonFromId _ = Nothing

data Click = Click {
      name :: Text
    , inst :: Text
    , button :: Int
    , x :: Int
    , y :: Int
    } deriving (Generic, Show)

instance ToJSON Click where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Click where
  parseJSON = withObject "Click" $ \v -> Click
      <$> v .: "name"
      <*> v .: "instance"
      <*> v .: "button"
      <*> v .: "x"
      <*> v .: "y"
