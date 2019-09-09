module Config
  ( Config(..)
  )
where

import           Network.MPD                    ( PlaylistName )

data Config = Config
  { confVolStep :: Int
  , confAlbumShufflePlaylist :: Maybe PlaylistName }
