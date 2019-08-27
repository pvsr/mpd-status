{-# LANGUAGE OverloadedStrings #-}

module Operation(Operation(..), op) where

import Control.Monad (when)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.List as L (find, nub)

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
               | PreviousAlbum
               | Next
               | NextAlbum
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
op PreviousAlbum = previous
op NextAlbum = nextAlbum
-- really pining for the elegance of `mpc playlist -f %album% album-shuffle | uniq | sort -R` here
op AlbumShuffle = clear >> consume True >> random False >> listPlaylistInfo "album-shuffle" >>= (liftIO . Shuffle.shuffle . (queries . uniqAlbums)) >>= mapM_ findAdd >> play Nothing
  where uniqAlbums = L.nub . concat . mapMaybe (M.lookup Album . sgTags)
        queries = map (Album =?)

nextAlbum :: MPD ()
nextAlbum = let album = M.lookup Album . sgTags in do
  st <- status
  let position = stSongPos st
  when (isJust position) $ do
    let plLength = stPlaylistLength st
    pl <- playlistInfoRange $ Just (fromJust position, fromInteger plLength)
    -- TODO not safe
    let current = head pl
    let target = L.find (\song -> album song /= album current) pl
    play $ target >>= sgIndex

inc :: Int -> Int -> Int
inc step vol = min 100 $ (vol `div` step + 1) * step

dec :: Int -> Int -> Int
dec step vol = max 0 (baseN `div` step) * step
  where
    baseN = if vol `mod` step == 0
               then vol - 1
               else vol
