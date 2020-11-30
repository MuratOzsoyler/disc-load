module UI.Types where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (empty, Vector)

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

data InputEvent = 
        Closed 
        | OK 
        | Cancel 
        | Toggled Int32 Bool
        | TitleChanged Int32 Text 
        | FromChanged Int32 Text 
        | NotChanged
        deriving Show

data InputResult = InputResultRipDisc | InputResultSkipDisc
        deriving (Show, Eq)
