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
            { albumInfo = ItemInfo True "Üstad" "Münir Nurettin Selçuk" 
            , trackInfos = fromList
                [ ItemInfo True "Track 01" ""
                , ItemInfo True "Track 02" ""
                , ItemInfo True "Track 03" ""
                , ItemInfo True "Track 03" ""
                , ItemInfo True "Track 04" ""
                , ItemInfo True "Track 05" ""
                , ItemInfo True "Track 06" ""
                , ItemInfo True "Track 07" ""
                , ItemInfo True "Track 08" ""
                , ItemInfo True "Track 09" ""
                , ItemInfo True "Track 10" ""
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
printItemInfo = print
-- printItemInfo ItemInfo {..} = do
--     putStrLn 
--         $ "title=" <> pack (show title)
--         <> ", "
--         <> "from=" <> pack (show from)