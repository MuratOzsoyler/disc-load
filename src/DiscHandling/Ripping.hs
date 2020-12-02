module DiscHandling.Ripping where

import Prelude hiding (FilePath)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Vector (imapM_, Vector)
import Turtle (unsafeTextToLine, (%), d, echo, format
              , ExitCode (..), FilePath
              , s, shellStrictWithErr, textToLine, toText
              ) 

import UI.Types ( ItemInfo(..) )
import Utils (mkFileName,  shellQuote, showText )

writeTracks :: MonadIO m => FilePath -> Text -> Vector ItemInfo -> m ()
writeTracks dirName albumFrom trackInfos = do
    echo "Start of processing..."
    imapM_ sngTrackProcess trackInfos
    echo "End of processing"
  where
    sngTrackProcess n trackInfo@ItemInfo {..} = do
        if rip then do
            startOfTrackProcessing
            writeTrack
            endOfTrackProcessing
        else echo $ unsafeTextToLine $ showText trackIdx <> ". track is skipped."
      where
        trackIdx = n + 1
        writeTrack = do
            -- let (exitCode, cmd, _) = (ExitSuccess, format trackRipCmd trackIdx (shellQuote fileName), True)
            (exitCode, _, _) <- shellStrictWithErr (format trackRipCmd trackIdx (shellQuote fileNameToText)) mempty
            case exitCode of
                ExitFailure code -> echo . fromJust . textToLine
                    $ "Error creating file:"
                    <> showText code
                ExitSuccess -> return ()
        endOfTrackProcessing = echo " ...created."
        trackCnt             = length trackInfos
        trackRipCmd =
            "cdda2wav dev=/dev/cdrom -gui -cddb -1 -no-textfile -no-infofile -verbose-level disable -track " % d % " - | ffmpeg -i -y - " % s
        fileName = mkFileName trackIdx dirName albumFrom trackInfo  
        fileNameToText = either id id $ toText fileName
        -- either id id (toText 
        --     $ dirName
        --     </> fromText
        --             (format (i99 % ". " % s % " - " % s) 
        --                 trackIdx
        --                 (takeValue from albumFrom)
        --                 title
        --                 )
        --     <.> "m4a"
        --     )
        startOfTrackProcessing =
            echo 
                $ fromJust . textToLine
                $  "creating..."
                <> showText trackIdx
                <> "/"
                <> showText trackCnt
                <> " "
                <> fileNameToText
