module UI.Functions where

import UI.Types (ItemInfo (..), InputState (..))
import Control.Concurrent (MVar, readMVar, newEmptyMVar, newMVar)
import GI.Gtk (CheckButton (CheckButton), Orientation(OrientationHorizontal)
              , toWidget, Separator (Separator), Box (Box),  Widget, IsGrid, Align(..)
              , Entry (Entry), PositionType(..), Label (Label), Grid (Grid)
              , PolicyType(PolicyTypeAutomatic), ScrolledWindow (ScrolledWindow)
              , ApplicationWindow (ApplicationWindow), on, AttrOp((:=), (:=>)), new
              , Application(Application)
              )
import Control.Monad (forM, void)
import GI.Gio (applicationRun, GObject)
import DiscHandling.Utils (i99, mkPlaceHolder)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int32)
import Turtle (format)
import Data.Vector (toList)
import Control.Monad (forM_)

runInput :: InputState -> IO InputState
runInput input = do
    stateVar <- newMVar input

    app <- new Application [#applicationId := "disc-load"]
    void $ on app #activate $ appActivate app stateVar
    void $ applicationRun app Nothing
    
    readMVar stateVar

appActivate :: Application -> MVar InputState -> IO ()
appActivate app stateVar = do
    grid <- new Grid
        [ #columnSpacing := 2
        , #hexpand := True
        , #margin := 4
        , #rowSpacing := 2
        ]
    appWin <- new ApplicationWindow 
        [ #title := "Enter/Change CD Titles"
        , #application := app 
        , #child :=> new ScrolledWindow 
            [ #hscrollbarPolicy := PolicyTypeAutomatic
            , #vscrollbarPolicy := PolicyTypeAutomatic
            , #child := grid
            ]
            -- , On #deleteEvent $ const (False, Closed)
        ]
    InputState {..} <- readMVar stateVar

    gridLines <- 
        fmap (\(i, gcs) -> (\gc@GridChild {..} -> gc { top = i }) <$> gcs) 
        . zip [0..] 
        <$> (sequenceA [headerRow, albumSeparator, albumRow albumInfo, tracksSeparator]
            <> forM (zip [1..] $ toList trackInfos) trackRow
            )
    forM_ (concat gridLines) $ \GridChild {..} -> #attach grid widget left top width height
    let entryRows = filter  
    #showAll appWin
    
data GridChild = GridChild 
        { left :: Int32
        , top :: Int32
        , width :: Int32
        , height :: Int32
        , widget :: Widget
        }

type GridChildren = [[GridChild]]
        
headerRow :: MonadIO m => m [GridChild]
headerRow = sequenceA  
    [ GridChild 1 0 2 1 <$> (toWidget =<< new Label [#label := "Title"])
    , GridChild 3 0 1 1 <$> (toWidget =<< new Label [#label := "From"])
    ]

albumSeparator :: MonadIO m => m [GridChild]
albumSeparator = separatorRow "Album"

albumRow :: MonadIO m => ItemInfo -> m [GridChild]
albumRow = itemRow "Album" 1

tracksSeparator :: MonadIO m => m [GridChild]
tracksSeparator = separatorRow "Titles"

trackRow :: MonadIO m => (Int, ItemInfo) -> m [GridChild]
trackRow (idx, info) = do
    wdt <- toWidget =<< new Label [#label := format i99 idx]
    (GridChild 0 0 1 1 wdt :) <$> itemRow "track" 1 info

itemRow 
    :: MonadIO m 
    => Text 
    -> Int32 
    -> ItemInfo 
    -> m [GridChild]
itemRow itemType col ItemInfo {..} = sequenceA
    [ GridChild col 0 1 1 <$> (toWidget =<< new CheckButton [#active := rip])
    , GridChild (col + 1) 0 1 1 <$> (toWidget =<< inputEntry (mkPlaceHolder itemType "title") title)
    , GridChild (col + 2) 0 1 1 <$> (toWidget =<< inputEntry (mkPlaceHolder itemType "from") from)
    ]

inputEntry :: MonadIO m => Text -> Text -> m Entry
inputEntry plcHolder value = new Entry 
    [ #hexpand := True
    , #halign := AlignFill
    , #placeholderText := plcHolder
    , #text := value
    , #valign := AlignCenter
    -- , onM #changed $ \entry -> do
    --     newVal <- get entry #text
    --     return $ EntryChanged $ strip newVal
    ]

separatorRow :: MonadIO m => Text -> m [GridChild]
separatorRow gTitle = sequenceA
    [ GridChild 0 0 4 1 <$> do 
        box <- new Box 
            [ #hexpand := True
            , #orientation := OrientationHorizontal
            , #spacing := 2
            ]
        new Label [#label := gTitle] >>= \l -> #packStart box l False False 1
        new Separator                         
            [ #hexpand := True
            , #valign := AlignCenter
            , #orientation := OrientationHorizontal
            ] >>= \s -> #packStart box s True True 1
        toWidget box
    ]
