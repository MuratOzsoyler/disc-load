module UI.Types where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Vector (empty, Vector)
import Data.Functor.Identity (Identity)

data ItemInfo = ItemInfo 
        { title :: Text
        , from :: Text
        }
        deriving Show

emptyItemInfo :: ItemInfo
emptyItemInfo = ItemInfo mempty mempty

data InputState = InputState
        { albumInfo :: ItemInfo
        , trackInfos :: Vector ItemInfo
        , sanitize :: InputState -> InputState
        , expandFillIdx :: Int
        }
        -- deriving Show

emptyInputState :: InputState
emptyInputState = InputState emptyItemInfo empty id 0

data InputEvent = Closed | Escaped | Clicked | OK | Cancel | Changed Int32 Int32 Text | NotChanged
        deriving Show
