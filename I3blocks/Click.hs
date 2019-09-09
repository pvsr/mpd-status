{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module I3blocks.Click (Button(..)) where

import Data.Aeson

data Button = LeftClick
            | MiddleClick
            | RightClick
            | ScrollUp
            | ScrollDown
            | ScrollLeft
            | ScrollRight
            | Back
            | Forward
            deriving (Show)

instance FromJSON Button where
  parseJSON = withScientific "Button" $ \case
      1 -> pure LeftClick
      2 -> pure MiddleClick
      3 -> pure RightClick
      4 -> pure ScrollUp
      5 -> pure ScrollDown
      6 -> pure ScrollLeft
      7 -> pure ScrollRight
      8 -> pure Back
      9 -> pure Forward
      _ -> fail "expected an integer in [1, 9]"
