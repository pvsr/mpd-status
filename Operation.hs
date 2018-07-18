{-# LANGUAGE OverloadedStrings #-}
module Operation(Operation(..), op) where

import Network.MPD
import Network.MPD.Commands.Extensions

data Operation = Toggle
               | Stop
               | VolumeUp Int
               | VolumeDown Int
               | Mute
               | Previous
               | Next
               | AllRandom

op :: Maybe Operation -> MPD ()
op (Just Toggle) = toggle
op (Just AllRandom) = clear >> add "" >> random True >> play Nothing
op (Just Stop) = stop
op (Just (VolumeUp volStep)) = status >>= maybe (return ()) (setVolume . inc volStep) . stVolume
op (Just (VolumeDown volStep)) = status >>= maybe (return ()) (setVolume . dec volStep) . stVolume
-- TODO it would be nice to be able to toggle mute. is that info stored?
op (Just Mute) = setVolume 0
op (Just Previous) = previous
op (Just Next) = next
op Nothing = return ()

inc :: Int -> Int -> Int
inc step vol = min 100 $ (vol `div` step + 1) * step

dec :: Int -> Int -> Int
dec step vol = max 0 (baseN `div` step) * step
  where
    baseN = if vol `mod` step == 0
               then vol - 1
               else vol
