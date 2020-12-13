{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
module UI.Functions where

import Prelude as Prelude

import Control.Concurrent (takeMVar, putMVar, modifyMVar_, modifyMVar, MVar, readMVar, newMVar)
import Control.Monad ((<=<), forM_, forM, unless, void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Primitive (PrimMonad(PrimState))
import Control.Monad.Reader (ask, asks, runReaderT, ReaderT)
import Control.Monad.State as State (get, gets, modify, runStateT, StateT)
import Control.Monad.Trans (MonadTrans(lift))
import Data.Char (isSpace)
import Data.Int (Int32)
import Data.List.Extra (snoc)
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.Monoid (All(..), Endo(..))
import Data.Text as Text (dropEnd, breakOnEnd, count, replace, stripEnd, unpack, drop, breakOn, length, strip, last, init, null, Text)
import Data.Tuple.Extra (snd3, fst3)
import Data.Vector as Vector (MVector, length, (!), modify, toList)
import Data.Vector.Mutable as MVector (modify)

import Data.GI.Base.Signals (disconnectSignalHandler, SignalHandlerId)
import GI.Gio (applicationRun)
import GI.GLib (idleAdd, pattern PRIORITY_DEFAULT_IDLE)

import GI.Gtk as Gtk (ToggleButton (ToggleButton), IsMenuButton, PopoverConstraint(PopoverConstraintWindow), ResizeMode(ResizeModeParent)
                     , Popover (Popover), ArrowType(ArrowTypeDown), MenuButton (MenuButton)
                     , PositionType (..), toggleButtonGetActive
                     , AttrOp(On), Button (Button), get, set
                     , editableSetPosition, editableGetPosition
                     , CheckButton (CheckButton), Orientation(..)
                     , Separator (Separator), Box (Box), Align(..)
                     , Entry (Entry), Label (Label), Grid (Grid)
                     , PolicyType(PolicyTypeAutomatic), ScrolledWindow (ScrolledWindow)
                     , ApplicationWindow (ApplicationWindow), on, AttrOp((:=), (:=>)), new
                     , Application(Application)
                     )
import Reactive.Banana (filterE, Event, stepper, unionWith)
import Reactive.Banana.Frameworks (reactimate', changes, reactimate, mapEventIO, compile, actuate, MomentIO)
import Reactive.Banana.GI.Gtk (signalE0, AttrOpBehavior((:==)), sink, attrB)
import Turtle as Turtle(FilePath, format, testfile)

import Utils (showText, fset, as, packStart, addCssClass, mkDirName', mkFileName'
             , defaultTrackTitle, defaultAlbumArtist, defaultAlbumTitle
             , event2Behavior, i99, mkPlaceHolder
             )
import Types (InputResult (..), ItemInfo (..), InputState (..))

runInput :: InputState -> IO InputState
runInput input = do
    defAlbumTitle <- defaultAlbumTitle
    -- let sanitized = sanitize defAlbumTitle input  
    stateVar <- newMVar input -- sanitized

    app <- new Application [#applicationId := "disc.load"]
    void $ on app #activate $ appActivate app defAlbumTitle stateVar
    void $ applicationRun app Nothing
    putStrLn "app quitted"
    readMVar stateVar

data EntryType = AlbumTitle | AlbumFrom | TrackTitle Int | TrackFrom Int
        deriving (Show, Eq)

data FileTest = FileTest {idx :: Int, path :: Turtle.FilePath}
        deriving Show

data ExtractType = ArtistTitle | TitleArtist
        deriving Enum
instance Show ExtractType where
    show = \case
        ArtistTitle -> "Artist then Title"
        TitleArtist -> "Title then Artist"

type RowWidgets = (CheckButton, Entry, Entry)

type Buttons = (MenuButton, Button, Button)

data RenderConfig = RenderConfig
        { stateVar :: MVar InputState
        , idleRepeatVar :: MVar Bool
        , fileTestVar :: MVar [FileTest]
        , defaultVar :: MVar [EntryType]
        , defAlbumTitle :: Text 
        , applicationWindow :: ApplicationWindow
        , grid :: Grid
        }

data RenderState = RenderState
        { gridRow :: Int
        , entryRows :: [(Int, RowWidgets)]
        , buttons :: Maybe Buttons
        , extractTypeVar :: MVar ExtractType
        }

type RenderM m a = ReaderT RenderConfig (StateT RenderState m) a

lift'' :: Monad m => m a -> RenderM m a
lift'' = lift . lift

runRenderM :: RenderConfig -> RenderState -> RenderM m a -> m (a, RenderState)
runRenderM config state action = runStateT (runReaderT action config) state

appActivate :: Application -> Text -> MVar InputState -> IO ()
appActivate app defAlbumTitle stVar = do
    idleVar <- newMVar True
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
        , On #deleteEvent $ const $ modifyMVar idleVar $ const $ return (False, False)
        ]

    checkVar <- newMVar []
    defaultVar <- newMVar []
    extractTypeVar <- newMVar ArtistTitle
    let renderConfig = RenderConfig 
            { stateVar = stVar
            , idleRepeatVar = idleVar
            , fileTestVar = checkVar
            , defaultVar = defaultVar
            , defAlbumTitle = defAlbumTitle
            , applicationWindow = appWin
            , grid = grid
            }
    let renderState = RenderState 
            { gridRow = 0
            , entryRows = []
            , buttons = Nothing
            , extractTypeVar = extractTypeVar
            }
    runRenderM renderConfig renderState $ do
        setUpGridChildren
        withAlbumTitleValue $ do
            checkFileFunc <- startTestFileExistence
            void $ idleAdd PRIORITY_DEFAULT_IDLE checkFileFunc
            defaultFunc <- startDefaultAssign
            void $ idleAdd PRIORITY_DEFAULT_IDLE defaultFunc
            config <- ask
            state <- State.get
            liftIO $ compile (() <$ runRenderM config state networkDefinition) >>= actuate
        return ()
    #showAll appWin

startTestFileExistence :: RenderM IO (IO Bool)
startTestFileExistence = do
    (albumRip, _, _) <- State.gets $ snd . (!! 0) . entryRows
    trackRips <- State.gets $ fmap (fst3 . snd) . Prelude.drop 1 . entryRows
    checkVar <- asks fileTestVar
    idleVar <- asks idleRepeatVar
    return $ do
        fileTest <- modifyMVar checkVar $ \checks -> 
            return (Prelude.drop 1 checks, listToMaybe checks)
        case fileTest of
            Nothing -> return ()
            Just FileTest {..} -> do
                exists <- testfile path
                set (trackRips !! idx) [#active := not exists]
                allActive <- checkAllTracksActive id trackRips
                allInactive <- checkAllTracksActive not trackRips
                let inconsistent = not allActive && not allInactive
                set albumRip [#inconsistent := inconsistent]
                unless inconsistent $ set albumRip [#active := allActive]
        readMVar idleVar

startDefaultAssign :: RenderM IO (IO Bool)
startDefaultAssign = do
    entries <- State.gets $ ((\(_, (_, t, f)) -> (t, f)) <$>) . entryRows
    let (albumTitle, albumFrom) = head entries
        trackEntries = Prelude.drop 1 entries
    defaultVar <- asks defaultVar
    idleVar <- asks idleRepeatVar
    defaultAlbumTitle <- asks defAlbumTitle
    let invalidEscapes = [("\\'", "'")] :: [(Text, Text)]
        replaceAll = appEndo 
            $ mconcat 
            $ Endo . uncurry Text.replace 
            <$> invalidEscapes

    return $ do
        mbEntry <- modifyMVar defaultVar (\defs -> return (Prelude.drop 1 defs, listToMaybe defs))
        when (isJust mbEntry) $ do
            let (entry, def) = case fromJust mbEntry of
                    AlbumTitle -> (albumTitle, defaultAlbumTitle)
                    AlbumFrom -> (albumFrom, defaultAlbumArtist)
                    TrackTitle i -> (fst $ trackEntries !! i, defaultTrackTitle)
                    TrackFrom i -> (snd $ trackEntries !! i, "")
            value <- Gtk.get entry #text
            let invExist = all ((> 0) . (`Text.count` value) . fst) invalidEscapes 
            if Text.null $ strip value 
                then set entry [#text := def]
                else when invExist $ set entry [#text := replaceAll value]
        readMVar idleVar    

setUpGridChildren :: MonadIO m => RenderM m ()
setUpGridChildren = do
    headerRow 0
    albumSeparator 1
    InputState {..} <- liftIO . readMVar =<< asks stateVar
    albumRow 2 albumInfo
    tracksSeparator 3
    forM_ (zip [0..] $ toList trackInfos) $ trackRow 4
    buttonRow $ fromIntegral $ Vector.length trackInfos + 4
            
-- reloads first entry's text property inorder to fire file existence tests
withAlbumTitleValue :: RenderM IO () -> RenderM IO () 
withAlbumTitleValue f = do
    fstEntry <- gets $ snd3 . snd . (!! 0) . entryRows
    value <- Gtk.get fstEntry #text
    set fstEntry [#text := "\65533"]
    f
    set fstEntry [#text := value]

networkDefinition :: RenderM MomentIO ()
networkDefinition = do 
    (albumRip, albumTitle, albumFrom) <- gets $ snd . (!! 0) . entryRows
    trackRowWidgets <- gets $ Prelude.drop 1 . entryRows
    let trackRips = map (fst3 . snd) trackRowWidgets
    ripHandlingDefinition albumRip trackRips
    -- entry sanitation
    entryHandlingDefinition AlbumTitle albumTitle
    entryHandlingDefinition AlbumFrom albumFrom
    let trackEntries = map (\(i, (_, t, f)) -> (i, t, f)) trackRowWidgets
    forM_ trackEntries $ \(i, t, f) -> do
        entryHandlingDefinition (TrackTitle i) t
        entryHandlingDefinition (TrackFrom i) f
    -- -- file existence tests
    let fromTextB = lift'' . flip attrB #text
    albumTitleB <- fromTextB albumTitle
    albumFromB <- fromTextB albumFrom
    let dirNameB = mkDirName' <$> albumTitleB <*> albumFromB
    checkVar <- asks fileTestVar
    forM_ trackEntries $ \(i, t, f) -> do
        trackTitleB <- fromTextB t
        trackFromB <- fromTextB f
        let fileNameB = mkFileName' (i + 1) <$> dirNameB <*> albumFromB <*> trackTitleB <*> trackFromB
        lift'' $ do
            chgE <- changes fileNameB
            reactimate'
                $ fmap (\fn -> modifyMVar_ checkVar (\fts -> return $ snoc fts (FileTest i fn)))
                <$> chgE

ripHandlingDefinition :: CheckButton -> [CheckButton] -> RenderM MomentIO () 
ripHandlingDefinition albumRip trackRips = do
    albumRipInitialValue <- Gtk.get albumRip #active
    trackRipInitialValues <- forM trackRips $ flip Gtk.get #active
    albumChkValueB <- lift'' . stepper albumRipInitialValue 
        =<< getCheckButtonActiveWhenClicked albumRip

    trackRipEs <- forM (zip [0..] trackRips) trackCheckButtonHandling -- getCheckButtonClicked
    let trackRipsE = foldr1 (unionWith const) trackRipEs   
    -- setting track rips with album rip value 
    lift'' . forM_ trackRips $ flip sink [#active :== albumChkValueB]
    -- setting album rip inconsistent according to all track rips
    isAllTrackRipsCheckedE <- allTrackActiveStatus id trackRips trackRipsE
    isAllTrackRipsUncheckedE <- allTrackActiveStatus not trackRips trackRipsE
    let initialAllChecked = allChecked id trackRipInitialValues
        initialAllUnchecked = allChecked not trackRipInitialValues
    isAllTrackRipsCheckedB <- lift'' $ event2Behavior initialAllChecked isAllTrackRipsCheckedE
    isAllTrackRipsUncheckedB <- lift'' $ event2Behavior initialAllUnchecked isAllTrackRipsUncheckedE
    let inconsistentB = (&&) <$> fmap not isAllTrackRipsCheckedB <*> fmap not isAllTrackRipsUncheckedB 
    lift'' $ do
        sink albumRip [#inconsistent :== inconsistentB]
        -- set album active true if all checked or false if all unchecked 
        reactimate $ set albumRip [#active := True] <$ filterE id isAllTrackRipsCheckedE
        reactimate $ set albumRip [#active := False] <$ filterE id isAllTrackRipsUncheckedE
    return ()

allChecked :: (b -> Bool) -> [b] -> Bool    
allChecked f = getAll . mconcat . (All . f <$>) 
    
entryHandlingDefinition :: EntryType -> Entry -> RenderM MomentIO ()    
entryHandlingDefinition entryType entry = do
    let idx = case entryType of
            TrackTitle i -> i
            TrackFrom i -> i
            _ -> -1
    stateVar <- asks stateVar
    defaultVar <- asks defaultVar
    liftIO $ modifyMVar_ defaultVar $ return . (`snoc` entryType)
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
        value <- {- (\t -> if Text.null t then defText else t) 
            . -} conditionalStrip 
            <$> Gtk.get entry #text
        set entry [#text := value]
        modifyMVar_ defaultVar $ return . (`snoc` entryType)
        let modifyTitle info = info { title = value }
            modifyFrom info = info { from = value }
            modifyIdx 
                :: PrimMonad m 
                => (ItemInfo -> ItemInfo) 
                -> MVector (PrimState m) ItemInfo 
                -> m ()
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
    return ()
        
getCheckButtonClicked :: CheckButton -> RenderM MomentIO (Event ())
getCheckButtonClicked = lift . lift . flip signalE0 #clicked
    
trackCheckButtonHandling :: (Int, CheckButton) -> RenderM MomentIO (Event ())
trackCheckButtonHandling (idx, cb) = do
    stateVar <- asks stateVar
    on cb #toggled $ do
        value <- Gtk.get cb #active
        let modifyItemInfo info@ItemInfo {..} = info { rip = value }
            modifyMVector :: PrimMonad m => MVector (PrimState m) ItemInfo -> m ()
            modifyMVector mv = MVector.modify mv modifyItemInfo idx
        void $ modifyMVar_ stateVar $ \state@InputState {..} ->
            return state 
                { trackInfos = Vector.modify modifyMVector trackInfos }
    getCheckButtonClicked cb
        
getCheckButtonActiveWhenClicked :: CheckButton -> RenderM MomentIO (Event Bool)
getCheckButtonActiveWhenClicked cb = lift . lift . mapEventIO (\_ -> Gtk.get cb #active) =<< getCheckButtonClicked cb
    
allTrackActiveStatus :: (Bool -> Bool) -> [CheckButton] -> Event a -> RenderM MomentIO (Event Bool) 
allTrackActiveStatus f rips evt = lift'' $ mapEventIO (\_ -> checkAllTracksActive f rips) evt --  (\_ -> getAll . mconcat <$> forM rips (fmap (All . f) <$> flip get #active))
    
checkAllTracksActive :: MonadIO m => (Bool -> Bool) -> [CheckButton] -> m Bool
checkAllTracksActive f rips = getAll . mconcat <$> mapM ((All . f <$>) . toggleButtonGetActive) rips
        
headerRow :: MonadIO m => Int32 -> RenderM m ()
headerRow row = do
    grid <- asks grid
    t <- new Label [#label := "Title"]
    f <- new Label [#label := "From"]
    #attach grid t 1 row 2 1
    #attach grid f 3 row 1 1

albumSeparator :: MonadIO m => Int32 -> RenderM m ()
albumSeparator = separatorRow "Album"

albumRow :: MonadIO m => Int32 -> ItemInfo -> RenderM m ()
albumRow row info = do
    flds <- itemRow "Album" row 1 info
    State.modify 
        $ \s -> s {entryRows =  entryRows s `snoc` (-1 :: Int, flds)}

tracksSeparator :: MonadIO m => Int32 -> RenderM m ()
tracksSeparator = separatorRow "Tracks"

trackRow :: MonadIO m => Int32 -> (Int, ItemInfo) -> RenderM m ()
trackRow startRow (idx, info) = do
    grid <- asks grid
    l <- liftIO $ new Label [#label := format i99 (idx + 1)]
    let row = startRow + fromIntegral idx
    #attach grid l 0 row 1 1
    flds <- itemRow "track" row 1 info
    State.modify $ \s -> s {entryRows = entryRows s `snoc` (idx, flds)}

buttonRow :: MonadIO m => Int32 -> RenderM m ()
buttonRow row = do
    upperGrid <- asks grid
    lowerGrid <- new Grid
        [ #columnSpacing := 2
        , #hexpand := True
        , #margin := 4
        , #rowSpacing := 2
        , #columnHomogeneous := True
        ]
    -- extractHandler <- getExtractHandler "/"
    extract <- new MenuButton
        [ #direction := ArrowTypeDown
        , #halign := AlignStart
        , #hexpand := True
        , #valign := AlignCenter
        , #vexpand := True
        ]
    po <- newPopover extract
    set extract [#popover := po]
    -- on extract #clicked $ do
    --     mbPo <- Gtk.get extract #popover
    --     putStrLn $ ("has popover? " ++) $ show $ isJust mbPo
    --     putStrLn $ ("this popover is that popover? " ++) $ show $ (po ==) $ fromJust mbPo
    --     putStrLn . ("use popover? " ++) . show =<< Gtk.get extract #usePopover

    set po [#relativeTo := extract]
    -- #add extract mb
    -- set mb [#alignWidget := extract]
    #attach lowerGrid extract 0 0 1 1 
    box <- new Box [ #hexpand := True
                    , #halign := AlignCenter
                    , #vexpand := True
                    , #valign := AlignCenter
                    , #spacing := 0
                    , #homogeneous := True
                    ]
    addCssClass "linked" box
    appWin <- asks applicationWindow
    stateVar <- asks stateVar
    ok <- new Button 
        [ #label := "Rip Disc"
        , On #clicked $ closeHandler appWin stateVar InputResultRipDisc
        ]
    cancel <- new Button 
        [ #label := "Skip Disc"
        , On #clicked $ closeHandler appWin stateVar InputResultSkipDisc
        ]
    #packStart box ok False False 0
    #packStart box cancel False False 0
    #attachNextTo lowerGrid box (Just extract) PositionTypeRight 1 1
    l <- new Label 
        [ #hexpand := True
        , #halign := AlignEnd
        , #vexpand := True
        , #valign := AlignCenter
        ] 
    #attachNextTo lowerGrid l (Just box) PositionTypeRight 1 1
    #attach upperGrid lowerGrid 0 row 4 1
    State.modify $ \s -> s {buttons = Just (extract, ok, cancel)}

newPopover :: (MonadIO m, IsMenuButton w) => w -> RenderM m Popover
newPopover btn = do
    pop <- new Popover 
        [ #modal := True
        , #transitionsEnabled := True
        , #position := PositionTypeBottom
        , #resizeMode := ResizeModeParent
        , #constrainTo := PopoverConstraintWindow
        ]
    box <- new Box [#orientation := OrientationVertical, #spacing := 0]
    liftIO . packStart False False 0 box =<< newExtrBtn btn
    liftIO . packStart False False 0 box 
        =<< new Separator [#orientation := OrientationHorizontal]
    forM_ (["/", "-", "+"] :: [Text]) $
        liftIO . packStart False False 0 box <=< newDelimBtn btn
    liftIO . packStart False False 0 box =<< newDelimEntry btn
    #add pop box
    #showAll box
    return pop

newExtrBtn ::  (MonadIO m, IsMenuButton w) => w -> RenderM m Button 
newExtrBtn _ = do
    extVar <- gets extractTypeVar
    extBtn <- liftIO $ new Button [#label :=> showText <$> readMVar extVar]
    on extBtn #clicked $ 
        set extBtn 
            [ #label :=> showText <$> modifyMVar extVar 
                (\case
                    ArtistTitle -> return (TitleArtist, TitleArtist)
                    TitleArtist -> return (ArtistTitle, ArtistTitle)
                )
            ] 
    return extBtn

newDelimBtn :: (MonadIO m, IsMenuButton w) => w -> Text -> RenderM m Button 
newDelimBtn btn delim = do
    clkHandler <- getExtractHandler delim
    liftIO $ new Button 
        [ #label := delim
        , On #clicked $ clkHandler >> toggleStatus btn
        ]

newDelimEntry :: (MonadIO m, IsMenuButton w) => w -> RenderM m Entry
newDelimEntry btn = do
    config <- ask
    state <- State.get
    signalId <- liftIO $ newMVar (Nothing :: Maybe SignalHandlerId)
    entry <- new Entry 
        [ #placeholderText := "Enter delimiter"
        , #secondaryIconName := "gtk-apply"
        , #secondaryIconSensitive := True
        , #secondaryIconActivatable := True
        ]
    void $ on entry #changed $ do 
        delim <- Gtk.get entry #text
        let isDelimFull = not $ Text.null delim
        set entry 
            [ #secondaryIconActivatable := isDelimFull
            , #secondaryIconSensitive := isDelimFull
            ]
        when isDelimFull $ do
            (clkHandler, _) <- runRenderM config state $ getExtractHandler delim
            maybe (return ()) (disconnectSignalHandler entry) =<< takeMVar signalId 
            putMVar signalId . Just =<<  on entry #iconRelease (\_ _ -> clkHandler >> toggleStatus btn)
    return entry

getExtractHandler :: MonadIO m => Text -> RenderM m (IO ())
getExtractHandler delim = do
    ers <- gets $ ((\(_, (_, t, f)) -> (t, f)) <$>) . entryRows
    extrVar <- gets extractTypeVar
    return $ do 
        extractType <- liftIO $ readMVar extrVar 
        let dlen = Text.length delim
        forM_ ers $ \(t, f) -> do
            title <- Gtk.get t #text
            from <- Text.strip <$> Gtk.get f #text
            when (Text.null from) $ do
                case extractType of
                    ArtistTitle -> do
                        let (pfx, sfx) = Text.breakOn delim title
                            sfx' = Text.stripEnd sfx
                        when (not (Text.null sfx') && sfx /= delim) $ do
                            set t [#text := Text.drop dlen sfx]
                            set f [#text := pfx]
                    TitleArtist -> do
                        let (pfx, sfx) = Text.breakOnEnd delim title
                            pfx' = Text.strip pfx
                        when (not (Text.null pfx') && pfx /= delim) $ do
                            set t [#text := Text.dropEnd dlen pfx]
                            set f [#text := sfx]

closeHandler :: ApplicationWindow -> MVar InputState -> InputResult -> IO ()
closeHandler appWin stateVar result = do
    modifyMVar_ stateVar $ \state@InputState {..} ->
        return state {inputResult = result}
    #close appWin

itemRow 
    :: MonadIO m 
    => Text 
    -> Int32 
    -> Int32 
    -> ItemInfo 
    -> RenderM m RowWidgets
itemRow itemType row startCol ItemInfo {..} = do
    grid <- asks grid
    chk <- new CheckButton [#active := rip]
    #attach grid chk startCol row 1 1
    entt <- inputEntry (mkPlaceHolder itemType "title") title
    #attach grid entt (startCol + 1) row 1 1
    entf <- inputEntry (mkPlaceHolder itemType "from") from
    #attach grid entf (startCol + 2) row 1 1
    return (chk, entt, entf)

inputEntry :: MonadIO m => Text -> Text -> m Entry
inputEntry plcHolder value = new Entry 
    [ #hexpand := True
    , #halign := AlignFill
    , #placeholderText := plcHolder
    , #text := value
    , #valign := AlignCenter
    ]

separatorRow :: MonadIO m => Text -> Int32 -> RenderM m ()
separatorRow gTitle row = do
    grid <- asks grid
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
    #attach grid box 0 row 4 1

toggleStatus :: (MonadIO m, IsMenuButton o) => o -> m ()
toggleStatus btn = btn `as` ToggleButton >>= fset [#active := False]