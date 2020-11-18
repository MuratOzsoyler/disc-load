{-# LANGUAGE TypeApplications #-}
module UI.Functions where

import Control.Monad (guard)
import Data.Default.Class (Default (def))
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, null, pack, strip)
import Data.Vector ((!), (!?), fromList, toList, Vector)
import qualified Data.Vector as Vector (concat, drop, map, modify)
import qualified Data.Vector.Mutable as MVector (modify)

import GI.Gtk.Declarative (fill, defaultBoxChildProperties, BoxChild(BoxChild)
                          , expand, widget, Attribute((:=)), bin, container, on
                          , onM
                          )
import GI.Gtk.Declarative.App.Simple (App(..), AppView, run, Transition(..))
import GI.Gtk (Label(Label), Box(Box), Orientation(OrientationHorizontal)
              , Separator(Separator), Align(AlignFill, AlignCenter), Entry(Entry)
              , Grid(Grid), ApplicationWindow(ApplicationWindow)
              , get, Button (Button), Window (Window), glibType, Widget, toWidget
              , unsafeCastTo, ScrolledWindow (ScrolledWindow), PolicyType(..)
              )
import GI.Gtk.Declarative.Container.Grid (width, leftAttach, topAttach
                                         , GridChild(GridChild), defaultGridChildProperties
                                         )

import DiscHandling.Utils
import UI.Types

import Debug.Trace

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
inputView state = bin
    ApplicationWindow
    [ #title := "Enter/Change CD Titles"
    , on #deleteEvent $ const (False, Closed)
    -- , #widthRequest := 400    
    -- , #heightRequest := 300
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
                [ GridChild def $ widget Label [#label := "Title"]
                , GridChild def {leftAttach = 1} $ widget Label [#label := "From"]
                ] 
            , itemView (1, "album", albumInfo state)
            , fromList 
                [GridChild 
                    def {topAttach = 2, leftAttach = 0, width = 2} 
                    $ container Box [#hexpand := True, #orientation := OrientationHorizontal, #spacing := 2]
                        [ widget Label [#label := 
                                            ("Tracks: idx=" <> pack (show idx) 
                                                <> ", boxChildExpand=" <> pack (show boxChildExpand) 
                                                <> ", boxChildFill=" <> pack (show boxChildFill)
                                                <> ", sepExpand=" <> pack (show sepExpand)
                                                <> ", sepFill=" <> pack (show sepFill)
                                            )
                                       ] 
                        , BoxChild def {fill = boxChildFill, expand = boxChildExpand}
                            $ widget Separator 
                                [ #hexpand := sepExpand
                                , #halign := sepFill
                                , #orientation := OrientationHorizontal
                                ] 
                        ]
                ]
            , Vector.concat $ map itemView $ zip3 [3..] (repeat "track") (toList $ trackInfos state)
            , fromList 
                [ GridChild 
                    def {topAttach = fromIntegral $ length (trackInfos state) + 3, leftAttach = 0, width = 2} 
                    $ container Box [#hexpand := True, #orientation := OrientationHorizontal, #spacing := 2]
                        [ widget Button 
                            [ #label := "Değiştir"
                            , on #clicked Clicked
                            ]
                        , widget Button 
                            [ #label := "Tamam"
                            , onM #clicked $ quitGUI OK
                            ]
                        , widget Button 
                            [ #label := "Vazgeç"
                            , onM #clicked $ quitGUI Cancel
                            ]
                        ]
                ]
            ]
  where
    idx = expandFillIdx state
    (boxChildExpand, boxChildFill, sepExpand, sepFill) = expandFillTable ! idx
    itemView :: (Int32, Text, ItemInfo) -> Vector (GridChild InputEvent)
    itemView (row, itemLabel, (ItemInfo {..})) = 
        [ gridChild 0 "title" title
        , gridChild 1 "from" from
        ]
      where
        gridChild col itemType value = GridChild 
            def { topAttach = row, leftAttach = col }
            $ entryWidget
          where
            entryWidget = widget Entry 
                [ #hexpand := True
                , #halign := AlignFill
                , #placeholderText := ("Enter \"" <> itemType <> "\" value for " <> itemLabel)
                , #text := value
                , onM #changed $ \entry -> do
                    newVal <- get entry #text
                    putStrLn $ "newValue=" <> show newVal
                    let newValue = strip newVal
                        result = if value == newValue 
                            then NotChanged
                            else Changed row col newValue
                    putStrLn $ "result=" <> show result
                    return result
                ]
    quitGUI :: InputEvent -> Button -> IO InputEvent
    quitGUI evt button = do
        typ <- glibType @ApplicationWindow
        -- wgt <- toWidget button 
        mbWgt <- #getAncestor button typ
        case mbWgt of
            Nothing -> error "Couldn't find app window"
            Just wgt -> do 
                win <- unsafeCastTo Window wgt 
                #close win
        return evt

inputUpdate :: InputState -> InputState -> InputEvent -> Transition InputState InputEvent
inputUpdate reverted state@InputState {..} = 
    let sanitized = sanitize state
    in \case
        Closed -> Exit
        Escaped -> Transition reverted (return $ Just Closed) 
        Clicked -> Transition (sanitized { expandFillIdx = (expandFillIdx + 1) `mod` 16 }) (return Nothing)
        OK -> Transition sanitized (return Nothing) -- (return $ Just Closed)
        Cancel -> Transition reverted (return Nothing) -- state  (return $ Just Escaped)
        NotChanged -> trace "not changed" $ Transition state (return Nothing)
        Changed row col value -> 
            let row' = fromIntegral row
            in  Transition 
                (sanitize $ case row' of
                    1 -> state { albumInfo = trace "modify album" $ modifyItemInfo albumInfo }
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
