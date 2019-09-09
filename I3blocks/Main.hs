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

import           Data.Aeson                     ( decodeStrict )
import qualified Data.ByteString               as B
                                                ( getLine )
import qualified Data.Text                     as T
                                                ( Text )
import qualified Data.Text.IO                  as T
                                                ( putStrLn )
import           Network.MPD

import           Operation
import           I3blocks.Block
import           I3blocks.Config

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  forever $ do
    run block
    tid <- forkIO . forever . run $ idle [PlayerS] >> block
    b   <- decodeStrict <$> B.getLine
    killThread tid
    withMPD . maybe (return ()) op $ b >>= buttonToOp

run :: MPD T.Text -> IO ()
run = withMPD >=> either print T.putStrLn
