module Main where

import Prelude hiding (putStrLn)
import Control.Monad (forM_, void, when)
import Control.Monad.Loops (untilM_)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack, Text, unpack)
import Data.Text.IO (putStrLn)
import Data.Vector (Vector, fromList)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Turtle ((</>), cd, echo, fromString, fromText, options, textToLine, toText, view)
-- import GI.Gtk
-- import GI.Gtk.Declarative
-- import GI.Gtk.Declarative.App.Simple

import DiscHandling.Inspection
import DiscHandling.Ripping
import DiscHandling.Utils
import UI.Functions
import UI.Interaction
import UI.Types

-- textInput :: Vector (Text, Text)
-- textInput = fromList 
--     [ ("Album title", "Album from")
--     , ("Track title 1", "Track from 1")
--     , ("Track title 2", "Track from 2")
--     , ("", "")
--     ]

programTitle :: Text
programTitle = "Audio CD Ripper"

main :: IO ()
main = do
    mbwd <- options (fromString $ unpack programTitle) optionsParser
    case mbwd of
        Just wd -> do
            cd wd
            putStrLn $ "Changed to directory: '" <> either id id (toText wd) <> "'\n"
        Nothing -> return ()

    hSetBuffering stdout NoBuffering
    putStrLn programTitle >> putStrLn ""

    flip untilM_ (not <$> getContinueConfirm) $ do
        ejectDisc
        promptDisc
        loadDisc
        info <- readDiscInfo
        -- maybe (return ()) view info
        when (isJust info) $ do
            discInfo@InputState {..} <- parseDiscInfo info
            print albumInfo
            print trackInfos
            discOutput <- runInput discInfo
            printDiscOutput discOutput

            ok <- getRipConfirm
            when ok $ do
                let InputState {..} = discOutput
                    dirName = mkDirName albumInfo
                echo $ fromMaybe "Invalid text!" $ textToLine $ shellQuote $ either id id $ toText dirName
                createDirectory $ shellQuote $ either id id $ toText dirName
                writeTracks dirName (from albumInfo) trackInfos

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