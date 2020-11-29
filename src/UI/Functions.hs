{-# LANGUAGE RecursiveDo #-}
module UI.Functions where

import UI.Types (ItemInfo (..), InputState (..))
import Control.Concurrent (MVar, readMVar, newMVar)
import GI.Gtk (CheckButton (CheckButton), Orientation(OrientationHorizontal)
              , toWidget, Separator (Separator), Box (Box),  Widget, Align(..)
              , Entry (Entry), Label (Label), Grid (Grid)
              , PolicyType(PolicyTypeAutomatic), ScrolledWindow (ScrolledWindow)
              , ApplicationWindow (ApplicationWindow), on, AttrOp((:=), (:=>)), new
              , Application(Application), castTo
              )
import Control.Monad (forM, forM_, void)
import GI.Gio (applicationRun)
import DiscHandling.Utils (as, i99, mkPlaceHolder)
import Data.Text (unpack, Text)
import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int32)
import Turtle ((%), d, s, format)
import Data.Vector (toList)
import System.IO.Unsafe (unsafePerformIO)
import Control.Applicative (Alternative((<|>)))
import Data.Functor (($>))
import Data.List (intercalate, uncons)
import Data.Maybe (fromJust, fromMaybe)
import Reactive.Banana.Frameworks (reactimate, mapEventIO, compile, actuate, MomentIO)
import Reactive.Banana.GI.Gtk (signalE0, AttrOpBehavior((:==)), sink, attrB, attrE)
import Data.List.Extra (takeEnd)
import Reactive.Banana (filterE, Event, (<@), stepper, unionWith, accumB, whenE)
import GI.Gtk (get)
import Data.Tuple.Extra (fst3)
import GI.Gtk (set)
import Data.Monoid (All(..))
import Control.Monad.IO.Class (MonadIO(liftIO))

runInput :: InputState -> IO InputState
runInput input = do
    stateVar <- newMVar input

    app <- new Application [#applicationId := "disc.load"]
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
    let entryRows = takeEnd 3 <$> filter ((>= 3) . length) gridLines
    print entryRows
    compile (networkDefinition entryRows) >>= actuate
    #showAll appWin
  where
    networkDefinition :: GridChildren -> MomentIO ()
    networkDefinition entryRows = mdo 
        let (albumRow :: [GridChild], trackRows :: GridChildren) = fromJust $ uncons entryRows
        (albumChk, albumTitle, albumFrom) <- itemRowWidgets albumRow
        albumRipInitialValue <- get albumChk #active
        trackRowWidgets <- zip [0..] <$> forM trackRows itemRowWidgets
        let trackRips = map (fst3 . snd) trackRowWidgets
        trackRipInitialValues <- forM trackRips $ flip get #active
        albumChkValueB <- stepper albumRipInitialValue =<< getCheckButtonActiveWhenClicked albumChk

        trackRipEs <- forM trackRips getCheckButtonClicked
        let trackRipsE = foldr1 (unionWith const) trackRipEs   
        -- setting track rips with album rip value 
        forM_ trackRips $ flip sink [#active :== albumChkValueB]
        -- setting album rip inconsistent according to all track rips
        isAllTrackRipsCheckedE <- allTrackActiveStatus id trackRips trackRipsE
        isAllTrackRipsUncheckedE <- allTrackActiveStatus not trackRips trackRipsE
        let initialAllChecked = getAll . mconcat $ All <$> trackRipInitialValues
            initialAllUnchecked = getAll . mconcat $ All . not <$> trackRipInitialValues
        liftIO $ print $ "initialAllChecked=" ++ show initialAllChecked
        liftIO $ print $ "initialAllUnchecked=" ++ show initialAllUnchecked
        isAllTrackRipsCheckedB <- event2Behavior initialAllChecked isAllTrackRipsCheckedE
        isAllTrackRipsUncheckedB <- event2Behavior initialAllUnchecked isAllTrackRipsUncheckedE
        let inconsistentB = (&&) <$> fmap not isAllTrackRipsCheckedB <*> fmap not isAllTrackRipsUncheckedB 
        sink albumChk [#inconsistent :== inconsistentB]
        -- set album active true if all checked or false if all unchecked 
        reactimate $ set albumChk [#active := True] <$ filterE id isAllTrackRipsCheckedE
        reactimate $ set albumChk [#active := False] <$ filterE id isAllTrackRipsUncheckedE
        return ()
    getCheckButtonClicked :: CheckButton -> MomentIO (Event ())
    getCheckButtonClicked = flip signalE0 #clicked
    getCheckButtonActiveWhenClicked :: CheckButton -> MomentIO (Event Bool)
    getCheckButtonActiveWhenClicked cb = mapEventIO (\_ -> get cb #active) =<< getCheckButtonClicked cb
    allTrackActiveStatus f rips = mapEventIO  (\_ -> getAll . mconcat <$> forM rips (fmap (All . f) <$> flip get #active))
    itemRowWidgets :: MonadIO m => [GridChild] -> m (CheckButton, Entry, Entry)
    itemRowWidgets row = do
        let [b, e1, e2] = row
        b' <- getWidgetFrom b `as` CheckButton
        e1' <- getWidgetFrom e1 `as` Entry
        e2' <- getWidgetFrom e2 `as` Entry
        return (b', e1', e2')
      where
        getWidgetFrom GridChild {..} = widget 
    event2Behavior initial = stepper initial
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
tracksSeparator = separatorRow "Tracks"

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
