{-# LANGUAGE TypeApplications #-}
module UI.Functions where

import Data.Default.Class (Default(def))
import Data.Functor ((<&>))
import Data.Text (strip, Text)
import Data.Vector (toList, fromList)
import GI.Gtk 
        ( Align(AlignEnd, AlignFill)
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
import GI.Gtk.Declarative.Container.Grid (topAttach, leftAttach, width, GridChild(..))
import UI.Types (ItemInfo (..), InputEvent (..), InputState (..))
import DiscHandling.Utils (showText)

runInput :: InputState -> IO InputState
runInput = run . mkApp

mkApp :: InputState -> App ApplicationWindow InputState InputEvent
mkApp state = App
    { view = inputView
    , update = inputUpdate
    , inputs = mempty
    , initialState = state 
    }

inputUpdate :: InputState -> InputEvent -> Transition InputState InputEvent
inputUpdate InputState {..} = error "not implemented"
 
    
inputView :: InputState -> AppView ApplicationWindow InputEvent
inputView InputState {..} = bin
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
        $ (\(r, gs) -> map (\GridChild {..} -> GridChild (propTopAtch r properties) child) gs)
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
        [ gridChild (\p -> p {width = 2}) $ labelWgt "Title"
        , gridChild (\p -> p {leftAttach = 2}) $ labelWgt "From"
        ]
    albumSep :: [GridChild InputEvent]
    albumSep = genericSep "Album"
    albumEntryRow :: [GridChild InputEvent]
    albumEntryRow = 
        let ItemInfo {..} = albumInfo
        in  [ gridChild propWidth2 
                (entryWidget (mkPlaceHolder "title" "album") title 
                    <&> entEvt2InpEvt (TitleChanged (-1)) 
                    )
            , gridChild propLeftAtch2
                (entryWidget (mkPlaceHolder "from" "album") from
                    <&> entEvt2InpEvt (FromChanged (-1))
                    )
            ]
    tracksSep = genericSep "Tracks"
    trackInfoRows :: [[GridChild InputEvent]]
    trackInfoRows = zipWith 
        (\i ItemInfo {..} -> 
            [ gridChild id $ widget Label [#label := showText (i + 1), #halign := AlignEnd]
            , gridChild propLeftAtch1
                (entryWidget (mkPlaceHolder "title" "track") title
                    <&> entEvt2InpEvt (TitleChanged i)
                    ) 
            , gridChild propLeftAtch2 
                (entryWidget (mkPlaceHolder "from" "track") from
                    <&> entEvt2InpEvt (FromChanged i)
                    )
            ])
        [0..]
        $ toList trackInfos
    buttonsRow :: [[GridChild InputEvent]]
    buttonsRow =
        [[ gridChild propWidth2 
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
        [ gridChild (\p -> p {width = 3}) 
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
    entEvt2InpEvt inpEvtCons (EntryChanged val) = inpEvtCons val
    mkPlaceHolder typ label = "Enter \"" <> typ <> "\" value for " <> label
    labelWgt lbl = widget Label [#label := lbl]
    gridChild propf = GridChild (propf def)
    propWidth2 p = p { width = 2 } 
    propLeftAtch1 p = p { leftAttach = 1 } 
    propLeftAtch2 p = p { leftAttach = 2 } 
    propTopAtch r p = p { topAttach = r } 

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
