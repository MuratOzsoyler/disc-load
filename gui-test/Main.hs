module Main where

import Prelude hiding (putStrLn)
import Control.Monad (forM_)
import Data.Text (pack, Text)
import Data.Text.IO (putStrLn)

import UI.Functions ( runInput )
import UI.Types (emptyInputState,  InputState(..), ItemInfo(..) )
import Data.Vector (fromList)

programTitle :: Text
programTitle = "Test Audio CD Ripper GUI"

main :: IO ()
main = do
    let discInfo@InputState {..} = emptyInputState 
            { albumInfo = ItemInfo "Üstad" "Münir Nurettin Selçuk" 
            , trackInfos = fromList
                [ ItemInfo "Track 01" ""
                , ItemInfo "Track 02" ""
                , ItemInfo "Track 03" ""
                , ItemInfo "Track 03" ""
                , ItemInfo "Track 04" ""
                , ItemInfo "Track 05" ""
                , ItemInfo "Track 06" ""
                , ItemInfo "Track 07" ""
                , ItemInfo "Track 08" ""
                , ItemInfo "Track 09" ""
                , ItemInfo "Track 10" ""
                ]
            } 
    print albumInfo
    print trackInfos
    discOutput@InputState {..} <- runInput discInfo
    printDiscOutput discOutput

printDiscOutput :: InputState -> IO ()
printDiscOutput InputState {..} = do
    putStrLn "Album Info"
    printItemInfo albumInfo
    putStrLn "Track Infos"
    forM_ trackInfos printItemInfo 

printItemInfo :: ItemInfo -> IO ()
printItemInfo ItemInfo {..} = do
    putStrLn 
        $ "title=" <> pack (show title)
        <> ", "
        <> "from=" <> pack (show from)