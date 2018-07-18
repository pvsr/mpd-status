{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import Data.Aeson (decodeStrict)
import Data.Binary (encode)
import qualified Data.ByteString as B (ByteString, putStrLn, getLine, pack)
import Data.ByteString.Lazy (toStrict)
import Network.MPD
import Network.MPD.Commands.Extensions
import qualified Text.Show.ByteString as B (show)

import Button (button)

data Operation = Toggle
               | Stop
               | VolumeUp
               | VolumeDown
               | Mute
               | Previous
               | Next
               | AllRandom
               | None

buttonMap :: Maybe Int -> Operation
-- left button
buttonMap (Just 1) = Toggle
-- middle button
buttonMap (Just 2) = AllRandom
-- right button
buttonMap (Just 3) = Stop
-- scroll up
buttonMap (Just 4) = VolumeUp
-- scroll down
buttonMap (Just 5) = VolumeDown
-- back button
buttonMap (Just 8) = Previous
-- forward button
buttonMap (Just 9) = Next
buttonMap _ = None

volStep = 5

op :: Operation -> MPD ()
op Toggle = toggle
op AllRandom = clear >> add "" >> random True >> play Nothing
op Stop = stop
op VolumeUp = status >>= maybe (return ()) (setVolume . inc volStep) . stVolume
op VolumeDown = status >>= maybe (return ()) (setVolume . dec volStep) . stVolume
-- TODO it would be nice to be able to toggle mute. is that info stored?
op Mute = setVolume 0
op Previous = previous
op Next = next
op None = return ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  sequence_ . Prelude.repeat $ do
    run block
    tid <- forkIO $ do
      sequence_ . Prelude.repeat . run $ idle [PlayerS] >> block
    msg <- B.getLine
    killThread tid
    print msg
    let raw = decodeStrict msg
    print raw
    let b = fmap button raw
    withMPD . op $ buttonMap b


run :: MPD B.ByteString -> IO ()
run m = do
  out <- withMPD m
  case out of
    (Left msg) -> print msg
    (Right l) -> B.putStrLn l

block :: MPD B.ByteString
block = maybe "mpd stopped" . mappend <$> extractSong <*> statusInfo

statusInfo :: MPD (Maybe B.ByteString)
statusInfo = fmap statusInfo' status
statusInfo' Status { stState = state, stVolume = vol } =
  case (state, vol) of
    (Stopped, _) -> Nothing
    (Playing, Nothing) -> Just ""
    (Playing, Just 100) -> Just ""
    (Playing, Just v) -> Just $ " [" <> volIndicator v <> "%]"
    (Paused, Nothing) -> Just " [paused]"
    (Paused, Just 100) -> Just " [paused]"
    (Paused, Just v) -> Just $ " [paused | " <> volIndicator v <> "%]"
  where
    volIndicator v = symbol v <> " " <> toStrict (B.show v)
    -- There's probably a more convenient way to represent UTF-8 literals...
    symbol v
      | v > 49 = B.pack [0xef, 0x80, 0xa8]
      | v > 0 = B.pack [0xef, 0x80, 0xa7]
      | otherwise = B.pack [0xef, 0x80, 0xa6]

extractSong :: MPD B.ByteString
extractSong = fmap extractSong' currentSong
extractSong' song =
  fromMaybe "no song" $
  do tags <- sgTags <$> song
     if null tags
       then fmap (toStrict . encode . toString . sgFilePath) song
       else do
         title <- extract =<< M.lookup Title tags
         artist <- extract =<< M.lookup Artist tags
         return $ artist <> " - " <> title
  where
    extract :: [Value] -> Maybe B.ByteString
    extract [] = Nothing
    extract (b:_) = Just $ toUtf8 b

inc :: Int -> Int -> Int
inc step volume = min 100 $ (volume `div` step + 1) * step

dec :: Int -> Int -> Int
dec step volume = max 0 (baseN `div` step) * step
  where
    baseN =
      if volume `mod` step == 0
        then volume - 1
        else volume
