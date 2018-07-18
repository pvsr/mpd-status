{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, killThread)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import Data.Aeson (decodeStrict)
import qualified Data.ByteString as B (getLine)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (putStrLn)
import Network.MPD

import Click
import Config
import Operation

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  sequence_ . Prelude.repeat $ do
    run block
    tid <- forkIO . sequence_ . Prelude.repeat . run $ idle [PlayerS] >> block
    json <- B.getLine
    killThread tid
    withMPD . op $ (button <$> decodeStrict json) >>= buttonFromId >>= buttonToOp


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
