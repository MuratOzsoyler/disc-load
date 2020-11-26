{-# LANGUAGE TypeApplications #-}
module UI.Functions where

import Data.Default.Class (Default(def))
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.Text as Text (null, strip, Text)
import Data.Vector as Vector (all, and, cons, fromList, map, modify, toList, Vector)
import qualified Data.Vector.Mutable as MVector (modify)
import GI.Gtk 
        (toggleButtonGetActive, CheckButton (CheckButton),  Align(AlignEnd, AlignFill)
        , ApplicationWindow (..)
        , Box(..)
        , Button (..)
        , Entry (..)
        , get
        , glibType
        , Grid (..)
        , Label (..)
        , Orientation(OrientationHorizontal)
        , PolicyType(PolicyTypeAutomatic)
        , ScrolledWindow (..)
        , Separator (..)
        , unsafeCastTo
        , Window (..)
        )
import GI.Gtk.Declarative 
        ( Attribute((:=))
        , bin
        , BoxChild (..)
        , container
        , expand
        , fill
        , on
        , onM
        , widget
        , Widget
        )
import GI.Gtk.Declarative.App.Simple (Transition (..), AppView, App (..), run)
import GI.Gtk.Declarative.Container.Grid (GridChildProperties, topAttach, leftAttach, width, GridChild(..))

import UI.Types (ItemInfo (..), InputEvent (..), InputState (..))
import DiscHandling.Utils (sanitize, defaultAlbumTitle, defaultAlbumArtist, defaultTrackTitle, showText)

-- import Debug.Trace

runInput :: InputState -> IO InputState
runInput state = run . mkApp state =<< defaultAlbumTitle

mkApp :: InputState -> Text -> App ApplicationWindow InputState InputEvent
mkApp state defaultAlbumTitle = 
    let sanitized = sanitize defaultAlbumTitle state
    in App
        { view = inputView defaultAlbumTitle
        , update = inputUpdate sanitized
        , inputs = mempty
        , initialState = sanitized 
        }

inputUpdate :: InputState -> InputState -> InputEvent -> Transition InputState InputEvent
inputUpdate initial state@InputState {..} = \case
    Closed -> Exit
    OK -> Transition state $ return $ Just Closed
    Cancel -> Transition initial $ return $ Just Closed
    Toggled idx value -> Transition
        (case idx of
            -1 -> 
                let state'@InputState {..} = modifyState (modifyRip value) idx
                in state' { trackInfos = Vector.map (modifyRip value) trackInfos } 
            _ -> 
                let state'@InputState {..} = modifyState (modifyRip value) idx
                in state' { albumInfo = modifyRip (calcAlbumChk (rip albumInfo) trackInfos) albumInfo }
            )
        $ return Nothing
    TitleChanged idx value -> 
        Transition (modifyState (modifyTitle value) idx) $ return Nothing
    FromChanged idx value -> 
        Transition (modifyState (modifyFrom value) idx) $ return Nothing
    NotChanged -> error "NotChanged handling undefined"
  where
    modifyRip value info = info { rip = value }
    modifyTitle value info = info { title = value }
    modifyFrom value info = info { from = value }
    modifyIdx idx f vec =  MVector.modify vec f $ fromIntegral idx
    modifyTrackInfos idx f = modify (modifyIdx idx f) trackInfos
    modifyState f = \case
        -1 -> state { albumInfo = f albumInfo }
        idx -> state { trackInfos = modifyTrackInfos idx f }
    calcAlbumChk value trackInfos = 
        let allChecked = Vector.all rip trackInfos
            allUnchecked = Vector.all (not . rip) trackInfos
        in if allChecked
            then True
            else if allUnchecked
                then False
                else value 
inputView :: Text -> InputState -> AppView ApplicationWindow InputEvent
inputView defaultAlbumTitle InputState {..} = bin
    ApplicationWindow
    [ #title := "Enter/Change CD Titles"
    , on #deleteEvent $ const (False, Closed)
    ]
    $ bin
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeAutomatic
        ]
    $ container
        Grid
        [#hexpand := True, #rowSpacing := 2, #columnSpacing := 2, #margin := 4]
    $ fromList 
        $ (concat :: [[GridChild InputEvent]] -> [GridChild InputEvent]) 
        $ (\(r, gs) -> Prelude.map (\GridChild {..} -> GridChild (propTopAtch r properties) child) gs)
        <$> zip [0..]
            ([ headers
            , albumSep
            , albumEntryRow
            , tracksSep
            ] 
            ++ trackInfoRows
            ++ buttonsRow
            )
  where
    headers :: [GridChild InputEvent]
    headers = 
        [ gridChild (propWidth2 . propLeftAtch1) $ labelWgt "Title"
        , gridChild propLeftAtch3 $ labelWgt "From"
        ]
    albumSep :: [GridChild InputEvent]
    albumSep = genericSep "Album"
    albumEntryRow :: [GridChild InputEvent]
    albumEntryRow = itemInfoRow 
        (-1)
        propLeftAtch1
        chkConsistent
        plcHolder
        defaultAlbumTitle
        defaultAlbumArtist
        albumInfo
      where
        plcHolder = mkPlaceHolder "album" 
        rips f = f . rip <$> trackInfos
        chkConsistent = [#inconsistent := not (Vector.and (rips id) || Vector.and (rips not))]
    tracksSep = genericSep "Tracks"
    trackInfoRows :: [[GridChild InputEvent]]
    trackInfoRows = 
        let plcHolder = mkPlaceHolder "track"
        in zipWith 
            (\i info -> 
                gridChild id (widget Label [#label := showText (i + 1), #halign := AlignEnd])
                -- : gridChild propLeftAtch1 (widget CheckButton [])
                : itemInfoRow i propLeftAtch1 [#inconsistent := False] plcHolder defaultTrackTitle "" info
                )
            [0..]
            $ toList trackInfos
    buttonsRow :: [[GridChild InputEvent]]
    buttonsRow =
        [[ gridChild (propWidth 4) 
            $ container Box [#hexpand := True, #orientation := OrientationHorizontal, #spacing := 2]
                [ widget Button 
                    [ #label := "Rip Disc"
                    , onM #clicked $ quitGUI OK
                    ]
                , widget Button 
                    [ #label := "Cancel"
                    , onM #clicked $ quitGUI Cancel
                    ]
                ]
        ]]
    quitGUI :: InputEvent -> Button -> IO InputEvent
    quitGUI evt button = do
        typ <- glibType @ApplicationWindow
        mbWgt <- #getAncestor button typ
        case mbWgt of
            Nothing -> error "Couldn't find app window"
            Just wgt -> do 
                win <- unsafeCastTo Window wgt 
                #close win
        return evt
    genericSep :: Text -> [GridChild InputEvent]
    genericSep title =
        [ gridChild (propWidth 4) 
            $ container 
                Box 
                [#hexpand := True, #orientation := OrientationHorizontal, #spacing := 2]
                [ widget Label [#label := title]
                , BoxChild def {fill = True, expand = True}
                    $ widget Separator 
                        [ #hexpand := False
                        , #halign := AlignFill
                        , #orientation := OrientationHorizontal
                        ] 
                ]
        ]
    entEvt2InpEvt inpEvtCons defVal (EntryChanged val) = 
        let val' = if Text.null val then defVal else strip val
        in inpEvtCons val'
    itemInfoRow 
        :: Int32 
        -> (GridChildProperties -> GridChildProperties) 
        -> Vector (Attribute CheckButton InputEvent)
        -> (Text -> Text) 
        -> Text 
        -> Text 
        -> ItemInfo 
        -> [GridChild InputEvent]
    itemInfoRow idx fstProp cbxProps plcHolder defTitle defFrom ItemInfo {..} = 
        [ gridChild fstProp
            $ widget CheckButton 
                ((#active := rip) 
                `cons` (onM #toggled handler `cons` cbxProps)
                )
        , gridChild (propNextAtch 1 . fstProp)
            (inputWidget (TitleChanged idx) (plcHolder "title") defTitle title) 
        , gridChild (propNextAtch 2 . fstProp) -- (propLeftAtch3) 
            (inputWidget (FromChanged idx) (plcHolder "from") defFrom from) 
        ]
      where
        handler chk = Toggled idx <$> toggleButtonGetActive chk  
    inputWidget :: (Text -> InputEvent) -> Text -> Text -> Text -> Widget InputEvent
    inputWidget constr plcHolder defaultValue value = 
        entryWidget plcHolder value
        <&> entEvt2InpEvt constr defaultValue
    mkPlaceHolder label typ = "Enter \"" <> typ <> "\" value for " <> label
    labelWgt lbl = widget Label [#label := lbl]
    gridChild :: (GridChildProperties -> GridChildProperties) -> Widget InputEvent -> GridChild InputEvent
    gridChild propf = GridChild (propf def)
    propWidth2 = propWidth 2
    propWidth3 = propWidth 3
    propLeftAtch1 = propLeftAtch 1
    propLeftAtch2 = propLeftAtch 2
    propLeftAtch3 = propLeftAtch 3
    propLeftAtch c p = p { leftAttach = c }
    propNextAtch n p = p { leftAttach = leftAttach p + n }
    propTopAtch r p = p { topAttach = r } 
    propWidth w p = p { width = w } 

data EntryChangeEvent = EntryChanged Text

entryWidget :: Text -> Text -> Widget EntryChangeEvent
entryWidget plcHolder value = widget Entry 
    [ #hexpand := True
    , #halign := AlignFill
    , #placeholderText := plcHolder
    , #text := value
    , onM #changed $ \entry -> do
        newVal <- get entry #text
        return $ EntryChanged $ strip newVal
    ]
