{-# LANGUAGE OverloadedStrings #-}

module Operation(Operation(..), op) where

import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)
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
               | AlbumShuffle (Maybe PlaylistName)

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
-- TODO handle bad playlist argument
op (AlbumShuffle (Just pl)) = listPlaylistInfo pl >>= albumShuffle
op (AlbumShuffle Nothing) = (filterSongs <$> listAllInfo "") >>= albumShuffle
  where filterSongs :: [LsResult] -> [Song]
        filterSongs ls = [song | (LsSong song) <- ls]

-- really pining for the elegance of `mpc playlist -f %album% album-shuffle | uniq | sort -R` here
albumShuffle :: [Song] -> MPD ()
albumShuffle songs = clear >> random False >> (liftIO . Shuffle.shuffle . queries $ uniqAlbums songs) >>= mapM_ findAdd >> play Nothing
  where uniqAlbums = L.nub . concat . mapMaybe (M.lookup Album . sgTags)
        queries = map (Album =?)

nextAlbum :: MPD ()
nextAlbum = let album = M.lookup Album . sgTags
                whenJust m f = maybe (return ()) f m
             in do
  st <- status
  let untilEnd pos = playlistInfoRange $ Just (pos, fromInteger $ stPlaylistLength st)
  maybePl <- maybe (return Nothing) (fmap Just . untilEnd) $ stSongPos st
  whenJust maybePl $ \pl -> do
    -- since position was not Nothing, pl is guaranteed nonempty
    let currentAlbum = album $ head pl
    let index = L.find (\s -> album s /= currentAlbum) pl >>= sgIndex
    if isJust index then play index else next

inc :: Int -> Int -> Int
inc step vol = min 100 $ (vol `div` step + 1) * step

dec :: Int -> Int -> Int
dec step vol = max 0 (baseN `div` step) * step
  where
    baseN = if vol `mod` step == 0
               then vol - 1
               else vol
