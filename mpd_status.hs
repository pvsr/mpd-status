{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Posix.Env.ByteString (getEnv)

import Data.Binary (encode)
import qualified Data.ByteString as B (ByteString, putStr, pack)
import Data.ByteString.Char8 (readInt)
import Data.ByteString.Lazy (toStrict)
import Network.MPD
import qualified Text.Show.ByteString as B (show)

main :: IO ()
main = do
  out <- withMPD main'
  case out of
    (Left msg) -> print msg
    (Right l) -> B.putStr l

main' :: MPD B.ByteString
main' = do
  initStatus <- status
  let initState = stState initStatus
  let initVol = stVolume initStatus
  button <- liftIO $ getEnv "BLOCK_BUTTON"
  (curState, curVol) <-
    case button of
      (Just env) -> do
        let cmd = readInt env
        op (fmap fst cmd) initState initVol
      Nothing -> return (initState, initVol)
  song <- currentSong
  let statusString = info curState curVol
  return $ maybe "mpd stopped" (extract song <>) statusString

op :: Maybe Int -> State -> Maybe Int -> MPD (State, Maybe Int)
op Nothing s v = return (s, v)
op (Just cmd) state vol =
  case cmd of
    1 ->
      case state of
        Playing -> pause True >> return (Paused, vol)
        Paused -> pause False >> return (Playing, vol)
        Stopped -> play Nothing >> return (Playing, vol)
    2 -> clear >> add "" >> random True >> play Nothing >> return (Playing, vol)
    3 -> stop >> return (Stopped, vol)
    4 -> setVolume' inc
    5 -> setVolume' dec
    8 -> previous >> return (Playing, vol)
    9 -> next >> return (Playing, vol)
    _ -> noChange
  where
    noChange = return (state, vol)
    setVolume' f =
      case vol of
        Just v -> setVolume (f v) >> return (state, Just $ f v)
        Nothing -> noChange

info :: State -> Maybe Int -> Maybe B.ByteString
info state vol =
  case (state, vol) of
    (Stopped, _) -> Nothing
    (Playing, Just 100) -> Just ""
    (Playing, Just v) -> Just $ " [" <> volIndicator v <> "%]"
    (Paused, Just 100) -> Just " [paused]"
    (Paused, Just v) -> Just $ " [paused | " <> volIndicator v <> "%]"
    (_, Nothing) -> Just ""
  where
    volIndicator v = symbol v <> " " <> toStrict (B.show v)
    -- There's probably a more convenient way to represent UTF-8 literals...
    symbol v
      | v > 49 = B.pack [0xef, 0x80, 0xa8]
      | v > 0 = B.pack [0xef, 0x80, 0xa7]
      | otherwise = B.pack [0xef, 0x80, 0xa6]

extract :: Maybe Song -> B.ByteString
extract song =
  fromMaybe "no song" $
  do tags <- sgTags <$> song
     if null tags
       then fmap (toStrict . encode . toString . sgFilePath) song
       else do
         title <- extract' =<< M.lookup Title tags
         artist <- extract' =<< M.lookup Artist tags
         return $ artist <> " - " <> title
  where
    extract' :: [Value] -> Maybe B.ByteString
    extract' [] = Nothing
    extract' (b:_) = Just $ toUtf8 b

inc :: Int -> Int
inc volume = min 100 $ (volume `div` 5 + 1) * 5

dec :: Int -> Int
dec volume = max 0 (base5 `div` 5) * 5
  where
    base5 =
      if volume `mod` 5 == 0
        then volume - 1
        else volume
