module I3blocks.ButtonMap
  ( buttonToOp
  )
where

import           Config
import           Operation
import           I3blocks.Click

import           Control.Monad.Reader           ( Reader
                                                , asks
                                                )

buttonToOp :: Button -> Maybe (Reader Config Operation)
buttonToOp = albumShuffleButtons

defaultButtons :: Button -> Maybe (Reader Config Operation)
defaultButtons ScrollUp   = Just $ VolumeUp <$> asks confVolStep
defaultButtons ScrollDown = Just $ VolumeDown <$> asks confVolStep
defaultButtons b          = pure <$> simpleButtons b
 where
  simpleButtons LeftClick   = Just Toggle
  simpleButtons MiddleClick = Just AllRandom
  simpleButtons RightClick  = Just Stop
  simpleButtons Back        = Just Previous
  simpleButtons Forward     = Just Next
  simpleButtons _           = Nothing

albumShuffleButtons :: Button -> Maybe (Reader Config Operation)
albumShuffleButtons MiddleClick =
  Just $ AlbumShuffle <$> asks confAlbumShufflePlaylist
albumShuffleButtons Forward = Just $ pure NextAlbum
albumShuffleButtons b       = defaultButtons b
