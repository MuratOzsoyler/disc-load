{-# LANGUAGE TypeApplications #-}
module UI.Functions where

import Data.Default.Class (Default (def))
import Data.Int ( Int32 )
import Data.Text as Text (Text, strip)
import Data.Vector as Vector ((!), concat, cons, fromList, imap
                             , modify, splitAt, toList, Vector
                             )
import qualified Data.Vector as Vector (map)
import qualified Data.Vector.Mutable as MVector (modify)

import GI.Gtk.Declarative (fill
                          , expand, widget, Attribute((:=)), bin, container, on
                          , onM, Widget
                          )
import GI.Gtk.Declarative.Container.Box (BoxChild(BoxChild))
import GI.Gtk.Declarative.Container.Grid (width, leftAttach, topAttach
                                         , GridChild (..)
                                         )
import GI.Gtk.Declarative.App.Simple (App(..), AppView, run, Transition(..))
import GI.Gtk (Label(Label), Box(Box), Orientation(OrientationHorizontal)
              , Separator(Separator), Align(AlignFill, AlignCenter, AlignEnd), Entry(Entry)
              , Grid(Grid), ApplicationWindow(ApplicationWindow)
              , get, Button (Button), Window (Window), glibType
              , unsafeCastTo, ScrolledWindow (ScrolledWindow), PolicyType(..)
              )

import DiscHandling.Utils (showText,  getSanitize )
import UI.Types
    ( InputEvent(..),
      InputResult(InputResultRipDisc),
      InputState(..),
      ItemInfo(..) )

expandFillTable :: Vector (Bool, Bool, Bool, Align)
expandFillTable = 
    [ (True,  True,  True,  AlignFill)
    , (True,  True,  True,  AlignCenter)
    , (True,  True,  False, AlignFill)
    , (True,  True,  False, AlignCenter)
    , (True,  False, True,  AlignFill)
    , (True,  False, True,  AlignCenter)
    , (True,  False, False, AlignFill)
    , (True,  False, False, AlignCenter)
    , (False, True,  True,  AlignFill)
    , (False, True,  True,  AlignCenter)
    , (False, True,  False, AlignFill)
    , (False, True,  False, AlignCenter)
    , (False, False, True,  AlignFill)
    , (False, False, True,  AlignCenter)
    , (False, False, False, AlignFill)
    , (False, False, False, AlignCenter)
    ]

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
        $ Vector.concat 
            [ fromList
                [ GridChild def {width = 2} $ widget Label [#label := "Title"]
                , GridChild def {leftAttach = 2} $ widget Label [#label := "From"]
                ] 
            , let (i, t) = Vector.splitAt 1 $ itemView 1 0 "album" albumInfo
                  i' = Vector.map (\GridChild {..} -> GridChild properties {width = 2} child) i
                  t' = Vector.map (\GridChild {..} -> GridChild properties {leftAttach = 2} child) t
              in i' <> t'
            , fromList 
                [GridChild 
                    def {topAttach = 2, leftAttach = 0, width = 3} 
                    $ container Box [#hexpand := True, #orientation := OrientationHorizontal, #spacing := 2]
                        [ widget Label [#label := "Tracks"]
                        , BoxChild def {fill = boxChildFill, expand = boxChildExpand}
                            $ widget Separator 
                                [ #hexpand := sepExpand
                                , #halign := sepFill
                                , #orientation := OrientationHorizontal
                                ] 
                        ]
                ]
            , Vector.concat 
                $ zipWith trackView [1..]
                $ zip3 [3..] (repeat "track") (toList trackInfos)
            , fromList 
                [ GridChild 
                    def {topAttach = fromIntegral $ length trackInfos + 3, leftAttach = 0, width = 2} 
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
                ]
            ]
  where
    idx = expandFillIdx
    (boxChildExpand, boxChildFill, sepExpand, sepFill) = expandFillTable ! idx
    trackView :: Int -> (Int32, Text, ItemInfo) -> Vector (GridChild InputEvent)
    trackView trackIdx (row, itemLabel, itemInfo) =
        Vector.imap 
            (\idx GridChild {..} -> GridChild (properties {leftAttach = fromIntegral idx}) child)
            $ GridChild 
                def {topAttach = row}
                (widget Label 
                    [ #label := showText trackIdx
                    , #halign := AlignEnd
                    ])
            `cons` 
                itemView row 1 itemLabel itemInfo
    itemView :: Int32 -> Int32 -> Text -> ItemInfo -> Vector (GridChild InputEvent)
    itemView row startCol itemLabel ItemInfo {..} = fromList $ map
        (\(col, typ, entry) -> gridChild 
            (props row col) 
            (entryWidget typ itemLabel row col entry)
            )
        (zip3 [startCol ..] ["title", "from"] [title, from])
      where
        props row col p = p { leftAttach = col, topAttach = row }
    gridChild propf widgetf = GridChild (propf def) widgetf
    entryWidget itemType itemLabel row col value = widget Entry 
        [ #hexpand := True
        , #halign := AlignFill
        , #placeholderText := ("Enter \"" <> itemType <> "\" value for " <> itemLabel)
        , #text := value
        , onM #changed $ \entry -> do
            newVal <- get entry #text
            let newValue = strip newVal
                result = if value == newValue 
                    then NotChanged
                    else Changed row col newValue
            return result
        ]
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
    
inputView' :: InputState -> AppView ApplicationWindow InputEvent
inputView' InputState {..} = bin
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
    $ 

inputUpdate :: InputState -> InputState -> InputEvent -> Transition InputState InputEvent
inputUpdate reverted state@InputState {..} = 
    let sanitized = sanitize state
    in \case
        Closed -> Exit
        Escaped -> Transition reverted (return $ Just Closed) 
        Clicked -> Transition (sanitized { expandFillIdx = (expandFillIdx + 1) `mod` 16 }) (return Nothing)
        OK -> Transition (sanitized { inputResult = InputResultRipDisc }) (return Nothing)
        Cancel -> Transition reverted (return Nothing)
        NotChanged -> Transition state (return Nothing)
        Changed row col value -> 
            let row' = fromIntegral row
            in  Transition 
                (sanitize $ case row' of
                    1 -> state { albumInfo = modifyItemInfo albumInfo }
                    _ | row' >= 3 && row' < length trackInfos + 3 ->
                        let idx = row' - 3
                        in state
                            { trackInfos = Vector.modify 
                                (\mvec -> MVector.modify
                                    mvec
                                    modifyItemInfo
                                    idx
                                )
                                trackInfos
                            }
                      | otherwise -> error $ "undefined row value = " <> show row'
                )
                (return Nothing)
          where
            modifyItemInfo info = case col of
                0 -> info { title = value }
                1 -> info { from = value }

runInput :: InputState -> IO InputState
runInput state = run . mkInput state =<< getSanitize

mkInput 
    :: InputState 
    -> (InputState -> InputState) 
    -> App ApplicationWindow InputState InputEvent
mkInput params sanitize = 
    let params' = params { sanitize = sanitize }
    in App
        { view = inputView
        , update = inputUpdate params'
        , inputs = mempty
        , initialState = params'
        }
