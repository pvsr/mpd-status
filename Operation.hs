{-# LANGUAGE OverloadedStrings #-}

module Operation(Operation(..), op) where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (nub)

import Control.Monad.Trans (liftIO)
import Network.MPD
import Network.MPD.Commands.Extensions

import Shuffle

data Operation = Toggle
               | Stop
               | VolumeUp Int
               | VolumeDown Int
               | Mute
               | Previous
               | Next
               | AllRandom
               | AlbumShuffle

op :: Operation -> MPD ()
op Toggle = toggle
op AllRandom = clear >> add "" >> random True >> play Nothing
op Stop = stop
op (VolumeUp volStep) = status >>= maybe (return ()) (setVolume . inc volStep) . stVolume
op (VolumeDown volStep) = status >>= maybe (return ()) (setVolume . dec volStep) . stVolume
-- TODO it would be nice to be able to toggle mute. is that info stored?
op Mute = setVolume 0
op Previous = previous
op Next = next
-- really pining for the elegance of `mpc playlist -f %album% album-shuffle | uniq | sort -R` here
op AlbumShuffle = clear >> consume True >> random False >> listPlaylistInfo "album-shuffle" >>= (liftIO . Shuffle.shuffle . (queries . uniqAlbums)) >>= mapM_ findAdd >> play Nothing
  where uniqAlbums = nub . concat . mapMaybe (M.lookup Album . sgTags)
        queries = map (Album =?)

inc :: Int -> Int -> Int
inc step vol = min 100 $ (vol `div` step + 1) * step

dec :: Int -> Int -> Int
dec step vol = max 0 (baseN `div` step) * step
  where
    baseN = if vol `mod` step == 0
               then vol - 1
               else vol
