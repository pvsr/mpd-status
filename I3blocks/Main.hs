import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Control.Monad                  ( forever
                                                , (>=>)
                                                )
import           System.IO                      ( stdout
                                                , hSetBuffering
                                                , BufferMode(LineBuffering)
                                                )

import           Control.Monad.Reader           ( Reader
                                                , runReader
                                                )
import           Data.Aeson                     ( decodeStrict )
import qualified Data.ByteString               as B
                                                ( getLine )
import qualified Data.Text                     as T
                                                ( Text )
import qualified Data.Text.IO                  as T
                                                ( putStrLn )
import           Network.MPD

import           Config
import           Operation
import           I3blocks.Block
import           I3blocks.ButtonMap
import           I3blocks.Click

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  forever $ do
    run block
    tid <- forkIO . forever . run $ idle [PlayerS] >> block
    b   <- decodeStrict <$> B.getLine
    killThread tid
    withMPD . maybe (return ()) op $ toOp b
 where
  toOp :: Maybe Button -> Maybe Operation
  toOp b = conf <$> (b >>= buttonToOp)
  conf :: Reader Config Operation -> Operation
  conf = flip
    runReader
    Config { confVolStep              = 5
           , confAlbumShufflePlaylist = Just $ PlaylistName "album-shuffle"
           }

run :: MPD T.Text -> IO ()
run = withMPD >=> either print T.putStrLn
