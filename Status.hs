{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import Data.Aeson (decodeStrict)
import qualified Data.ByteString as B (getLine)
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import Network.MPD
import Network.MPD.Commands.Extensions

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

volStep :: Int
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
    tid <- forkIO . sequence_ . Prelude.repeat . run $ idle [PlayerS] >> block
    json <- B.getLine
    killThread tid
    withMPD . op . buttonMap . fmap button $ decodeStrict json


run :: MPD T.Text -> IO ()
run m = do
  out <- withMPD m
  case out of
    (Left msg) -> print msg
    (Right l) -> T.putStrLn l

block :: MPD T.Text
block = maybe "mpd stopped" . mappend <$> extractSong <*> statusInfo

statusInfo :: MPD (Maybe T.Text)
statusInfo = fmap statusInfo' status
  where statusInfo' Status { stState = state, stVolume = vol } =
          case (state, vol) of
            (Stopped, _) -> Nothing
            (Playing, Nothing) -> Just ""
            (Playing, Just 100) -> Just ""
            (Playing, Just v) -> Just $ " [" <> volIndicator v <> "%]"
            (Paused, Nothing) -> Just " [paused]"
            (Paused, Just 100) -> Just " [paused]"
            (Paused, Just v) -> Just $ " [paused | " <> volIndicator v <> "%]"
          where
            volIndicator v = symbol v <> " " <> T.pack (show v)
            symbol v
              | v > 49 = "\xf028"
              | v > 0 = "\xf027"
              | otherwise = "\xf026"

extractSong :: MPD T.Text
extractSong = fmap extractSong' currentSong
  where extractSong' song =
          fromMaybe "no song" $
          do tags <- sgTags <$> song
             if null tags
               then fmap (toText . sgFilePath) song
               else do
                 title <- extract =<< M.lookup Title tags
                 artist <- extract =<< M.lookup Artist tags
                 return $ artist <> " - " <> title
        extract [] = Nothing
        extract (b:_) = Just $ toText b

inc :: Int -> Int -> Int
inc step vol = min 100 $ (vol `div` step + 1) * step

dec :: Int -> Int -> Int
dec step vol = max 0 (baseN `div` step) * step
  where
    baseN = if vol `mod` step == 0
               then vol - 1
               else vol
