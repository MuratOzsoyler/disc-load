module UI.Functions where

import Prelude as Prelude

import Control.Applicative (Alternative((<|>)))
import Control.Concurrent (modifyMVar_, modifyMVar, MVar, readMVar, newMVar)
import Control.Monad (forM, forM_, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Functor (($>))
import Data.Int (Int32)
import Data.List (intercalate, uncons)
import Data.List.Extra (takeEnd)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (All(..))
import Data.Text as Text (init, last, strip, length, null, unpack, Text)
import Data.Tuple.Extra (thd3, snd3, fst3)
import Data.Vector as Vector ((!), modify, toList)
import Data.Vector.Mutable as MVector (modify)
import System.IO.Unsafe (unsafePerformIO)

import GI.Gio (applicationRun)
import GI.Gtk (Button (Button), get, set, editableSetPosition, editableGetPosition
              , CheckButton (CheckButton), Orientation(OrientationHorizontal)
              , toWidget, Separator (Separator), Box (Box),  Widget, Align(..)
              , Entry (Entry), Label (Label), Grid (Grid)
              , PolicyType(PolicyTypeAutomatic), ScrolledWindow (ScrolledWindow)
              , ApplicationWindow (ApplicationWindow), on, AttrOp((:=), (:=>)), new
              , Application(Application), castTo
              )
import Reactive.Banana (filterE, Event, (<@), stepper, unionWith, accumB, whenE)
import Reactive.Banana.Frameworks (reactimate, mapEventIO, compile, actuate, MomentIO)
import Reactive.Banana.GI.Gtk (signalE0, AttrOpBehavior((:==)), sink, attrB, attrE)
import Turtle ((%), d, s, format)

import DiscHandling.Utils (defaultTrackTitle, defaultAlbumArtist, defaultAlbumTitle, event2Behavior, as, i99, mkPlaceHolder)
import UI.Types (ItemInfo (..), InputState (..))
import Data.Char (isSpace)
import Data.List.Extra (snoc)

runInput :: InputState -> IO InputState
runInput input = do
    stateVar <- newMVar input

    app <- new Application [#applicationId := "disc.load"]
    void $ on app #activate $ appActivate app stateVar
    void $ applicationRun app Nothing
    
    readMVar stateVar

data EntryType = AlbumTitle | AlbumFrom | TrackTitle Int | TrackFrom Int
        deriving (Show, Eq)

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
            <> buttonRow
            )
    forM_ (concat gridLines) $ \GridChild {..} -> #attach grid widget left top width height
    let entryRows = takeEnd 3 <$> filter ((>= 3) . Prelude.length) gridLines
    print entryRows
    compile (networkDefinition entryRows) >>= actuate
    #showAll appWin
  where
    networkDefinition :: GridChildren -> MomentIO ()
    networkDefinition entryRows = do 
        let (albumRow :: [GridChild], trackRows :: GridChildren) = fromJust $ uncons entryRows
        (albumRip, albumTitle, albumFrom) <- itemRowWidgets albumRow
        trackRowWidgets <- zip [0..] <$> forM trackRows itemRowWidgets
        let trackRips = map (fst3 . snd) trackRowWidgets
        ripHandlingDefinition albumRip trackRips
        defAlbumTitle <- defaultAlbumTitle
        -- entry sanitation
        entryHandlingDefinition AlbumTitle defAlbumTitle albumTitle
        entryHandlingDefinition AlbumFrom defaultAlbumArtist albumFrom
        let trackEntries = map (\(i, (_, t, f)) -> (i, t, f)) trackRowWidgets
            trackTitles = map (\(i, t, _) -> (i, t)) trackEntries
            trackFroms = map (\(i, _, f) -> (i, f)) trackEntries
        forM_ trackEntries $ \(i, t, f) -> do
            entryHandlingDefinition (TrackTitle i) defaultTrackTitle t
            entryHandlingDefinition (TrackFrom i) "" f

        -- return ()
    ripHandlingDefinition albumRip trackRips = do
        albumRipInitialValue <- get albumRip #active
        trackRipInitialValues <- forM trackRips $ flip get #active
        albumChkValueB <- stepper albumRipInitialValue =<< getCheckButtonActiveWhenClicked albumRip

        trackRipEs <- forM (zip [0..] trackRips) trackCheckButtonHandling -- getCheckButtonClicked
        let trackRipsE = foldr1 (unionWith const) trackRipEs   
        -- setting track rips with album rip value 
        forM_ trackRips $ flip sink [#active :== albumChkValueB]
        -- setting album rip inconsistent according to all track rips
        isAllTrackRipsCheckedE <- allTrackActiveStatus id trackRips trackRipsE
        isAllTrackRipsUncheckedE <- allTrackActiveStatus not trackRips trackRipsE
        let initialAllChecked = getAll . mconcat $ All <$> trackRipInitialValues
            initialAllUnchecked = getAll . mconcat $ All . not <$> trackRipInitialValues
        isAllTrackRipsCheckedB <- event2Behavior initialAllChecked isAllTrackRipsCheckedE
        isAllTrackRipsUncheckedB <- event2Behavior initialAllUnchecked isAllTrackRipsUncheckedE
        let inconsistentB = (&&) <$> fmap not isAllTrackRipsCheckedB <*> fmap not isAllTrackRipsUncheckedB 
        sink albumRip [#inconsistent :== inconsistentB]
        -- set album active true if all checked or false if all unchecked 
        reactimate $ set albumRip [#active := True] <$ filterE id isAllTrackRipsCheckedE
        reactimate $ set albumRip [#active := False] <$ filterE id isAllTrackRipsUncheckedE
    entryHandlingDefinition :: EntryType -> Text -> Entry -> MomentIO ()    
    entryHandlingDefinition entryType defText entry = do
        let idx = case entryType of
                TrackTitle i -> i
                TrackFrom i -> i
                _ -> -1
        on entry #changed $ do
            state@InputState {..} <- readMVar stateVar
            let oldValue = case entryType of
                    AlbumTitle -> title albumInfo
                    AlbumFrom -> from albumInfo
                    TrackTitle _ -> title $ trackInfos ! idx
                    TrackFrom _ -> from $ trackInfos ! idx
            position <- editableGetPosition entry
            let conditionalStrip newValue
                    | Text.null newValue = newValue
                    | oldValue == Text.init newValue && isSpace (Text.last newValue) = newValue 
                    | otherwise = strip newValue 
            value <- (\t -> if Text.null t then defText else t) 
                . conditionalStrip 
                <$> get entry #text
            set entry [#text := value]
            let modifyTitle info = info { title = value }
                modifyFrom info = info { from = value }
                modifyIdx f vec =  MVector.modify vec f $ fromIntegral idx
                modifyTrackInfos f = Vector.modify (modifyIdx f) trackInfos
                state' = case entryType of
                    AlbumTitle -> state { albumInfo = modifyTitle albumInfo }
                    AlbumFrom -> state { albumInfo = modifyFrom albumInfo }
                    TrackTitle _ -> state { trackInfos = modifyTrackInfos modifyTitle }
                    TrackFrom _ -> state { trackInfos = modifyTrackInfos modifyFrom }
            void $ modifyMVar stateVar $ \_ -> return (state', ()) 
            editableSetPosition entry $ if position >= fromIntegral (Text.length value) then -1 else position

        return ()
    getCheckButtonClicked :: CheckButton -> MomentIO (Event ())
    getCheckButtonClicked = flip signalE0 #clicked
    trackCheckButtonHandling (idx, cb) = do
        on cb #toggled $ do
            value <- get cb #active
            let modifyItemInfo info@ItemInfo {..} = info { rip = value }
                modifyMVector mv = MVector.modify mv modifyItemInfo idx
            void $ modifyMVar_ stateVar $ \state@InputState {..} ->
                return state 
                    { trackInfos = Vector.modify modifyMVector trackInfos }
        getCheckButtonClicked cb
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

buttonRow :: MonadIO m => m [[GridChild]]
buttonRow = 
    sequenceA 
        [ sequenceA
            [ GridChild 0 0 4 1 <$> do
                box <- new Box [#hexpand := False, #halign := AlignCenter, #spacing := 10]
                ok <- new Button [#label := "Rip Disc"]
                cancel <- new Button [#label := "Skip Disc"]
                #packEnd box ok False False 0
                #packEnd box cancel False False 0
                toWidget box
            ]
        ]
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
