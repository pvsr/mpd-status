{-# LANGUAGE OverloadedStrings #-}

module I3blocks.Config
  ( buttonToOp
  , volStep
  )
where

import           Operation
import           I3blocks.Click

import           Network.MPD                    ( PlaylistName(..) )

volStep :: Int
volStep = 5

buttonToOp :: Button -> Maybe Operation
buttonToOp LeftClick  = Just Toggle
--buttonToOp MiddleClick = Just AllRandom
buttonToOp RightClick = Just Stop
buttonToOp ScrollUp   = Just $ VolumeUp volStep
buttonToOp ScrollDown = Just $ VolumeDown volStep
buttonToOp Back       = Just Previous
--buttonToOp Forward = Just Next

-- TODO maybe there should be some notion of album mode vs single mode
buttonToOp MiddleClick =
  Just $ AlbumShuffle (Just $ PlaylistName "album-shuffle")
buttonToOp Forward = Just NextAlbum

buttonToOp _       = Nothing
