module DiscHandling.Ripping where

import Prelude hiding (FilePath)
import Control.Monad.IO.Class (MonadIO)
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Text (pack, Text)
import Data.Text.IO (putStrLn)
import Data.Vector (imapM_, indexed, Vector)
import Turtle ((<.>), (</>), (%), echo, format
              , fromText, ExitCode (..), FilePath, Format
              , s, shellStrictWithErr, textToLine, toText, UTCTime
              ) 
import Debug.Trace

import UI.Types
import DiscHandling.Utils

writeTracks :: MonadIO m => FilePath -> Text -> Vector (ItemInfo) -> m ()
writeTracks dirName albumFrom trackInfos = do
    echo "Start of processing..."
    imapM_ sngTrackProcess trackInfos
    echo "End of processing"
  where
    sngTrackProcess n ItemInfo {..} = do
            startOfTrackProcessing
            writeTrack
            endOfTrackProcessing
          where
            trackIdx = n + 1
            writeTrack = do
                (exitCode, _, _) <- shellStrictWithErr (trace $ format trackRipCmd trackIdx (shellQuote fileName)) mempty
                case exitCode of
                    ExitFailure code -> echo . fromJust . textToLine
                        $ "Error creating file:"
                        <> showText code
                    ExitSuccess -> return ()
            endOfTrackProcessing = echo " ...created."
            trackCnt             = length trackInfos
            trackRipCmd =
                "cdda2wav dev=/dev/cdrom -gui -cddb -1 -no-textfile -no-infofile -verbose-level disable -track " % i99 % " - | ffmpeg -i - " % s
            fileName = either id id (toText 
                $ dirName
                </> (fromText $ format (i99 % ". " % s % " - " % s) 
                        trackIdx
                        (takeValue from albumFrom)
                        title
                        )
                <.> "m4a"
                )
            startOfTrackProcessing =
                echo 
                    $ fromJust . textToLine
                    $  "creating..."
                    <> showText trackIdx
                    <> "/"
                    <> showText trackCnt
                    <> " "
                    <> fileName
