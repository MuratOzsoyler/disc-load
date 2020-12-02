module UI.Types where

import Data.Int (Int32)
import Data.Text (unpack, Text)
import Data.Vector (empty, Vector)
import GI.Gtk (CheckButton (CheckButton), Entry (Entry), castTo, Widget)
import Data.List (intercalate)
import Turtle ((%), d, s, format)
import System.IO.Unsafe (unsafePerformIO)
import Data.Functor (($>))
import Control.Applicative (Alternative((<|>)))
import Data.Maybe (fromMaybe)

data ItemInfo = ItemInfo 
        { rip :: Bool
        , title :: Text
        , from :: Text
        }
        deriving Show

emptyItemInfo :: ItemInfo
emptyItemInfo = ItemInfo True mempty mempty

data InputState = InputState
        { albumInfo :: ItemInfo
        , trackInfos :: Vector ItemInfo
        -- , sanitize :: InputState -> InputState
        , expandFillIdx :: Int
        , inputResult :: InputResult
        }
        deriving Show

emptyInputState :: InputState
emptyInputState = InputState emptyItemInfo empty {- id -} 0 InputResultSkipDisc

data InputResult = InputResultRipDisc | InputResultSkipDisc
        deriving (Show, Eq)

data GridChild = GridChild 
        { left :: Int32
        , top :: Int32
        , width :: Int32
        , height :: Int32
        , widget :: Widget
        }
instance Show GridChild where
    show GridChild {..} = '{' : intercalate ", " 
            [ showIntFld "left" left
            , showIntFld "top" top
            , showIntFld "width" width
            , showIntFld "height" height
            , showWidget
            ] ++ "}"
      where
        showIntFld name = unpack . format (s % ": " % d) name  
        showWidget = unsafePerformIO $ do 
            mbEntry <- ($> "Entry") <$> castTo Entry widget
            mbCheckButton <- ($> "CheckButton") <$> castTo CheckButton widget
            return $ fromMaybe "unknown widget" $ mbEntry <|> mbCheckButton

type GridChildren = [[GridChild]]
