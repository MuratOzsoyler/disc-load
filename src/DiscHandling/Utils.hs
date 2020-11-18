module DiscHandling.Utils where

import Prelude hiding (any, map, null, FilePath)
import Control.Applicative ((<|>), optional)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.Text (any, cons, null, pack, strip, takeEnd, Text, uncons, unpack)
import Data.Time (defaultTimeLocale, FormatTime, formatTime, getZonedTime)
import Data.Vector(map, Vector)
import Turtle ((%), (</>), echo, ExitCode (..), FilePath, Format, format, fromText
              , makeFormat, optPath, Parser, s, shell, textToLine, unsafeTextToLine
              , UTCTime
              )
import UI.Types

unicodeReplChar :: Char
unicodeReplChar = '\xFFFD'

ejectDisc, loadDisc :: MonadIO m => m () 

ejectDisc = execCmdOrExit
    "eject" 
    "Disc ejected..."
    "Error ejecting disc"

loadDisc = execCmdOrExit 
    "eject -t" 
    "Disc loaded." 
    "Error loading disc: "

createDirectory :: MonadIO m => Text -> m () 
createDirectory dirName = execCmdOrExit 
    ("mkdir -p " <> dirName)
    (format ("Directory '" % s % "' created.") dirName)
    ("Error creating directory")

execCmdOrExit :: MonadIO m => Text -> Text -> Text -> m ()
execCmdOrExit cmd successMsg failMsg = do
    exc <- shell cmd mempty
    case exc of
        ExitSuccess -> do
            echo $ unsafeTextToLine successMsg  
            return ()
        ExitFailure err -> error $ unpack failMsg ++ ": " ++ show err

shellQuote :: Text -> Text
shellQuote "" = "\"\""
shellQuote txt =
    let need_to_quote = (`elem` ("' \t\n\"\\|&;()<>!{}*[?]^$`#" :: [Char]))
    in  if any need_to_quote txt then '"' `cons` quote0' txt else txt
  where
    quote0' :: Text -> Text
    quote0' zs 
        | null zs = "\""
        | Just (z, zs') <- uncons zs =
            if z `elem` ("\"$`\\" :: [Char])
                then '\\' `cons` z `cons` quote0' zs'
                else z `cons` quote0' zs'
    -- quote0' (z:zs) = if z `elem` ("\"$`\\" :: [Char])
    --     then '\\' `cons` z `cons` quote0' zs
    --     else z : quote0' zs
    -- quote0' "" = "\""

showText :: Show a => a -> Text
showText = pack . show

ymd_hms :: FormatTime t => Format r (t -> r)
ymd_hms = makeFormat fmt 
  where
    fmt = pack . formatTime defaultTimeLocale "%Y%m%d_%H%M%S"

i99 :: (Integral i, Show i) => Format r (i -> r)
i99 = makeFormat fmt
  where
    fmt = takeEnd 2 . ("00" <>)  . showText

takeValue :: Text -> Text -> Text
takeValue x y = fromJust $ toMaybe x <|> toMaybe y

toMaybe :: Text -> Maybe Text
toMaybe x = if null x then Nothing else Just x

defaultAlbumTitle :: MonadIO m => m Text
defaultAlbumTitle = format ("Unknown Album " % ymd_hms) <$> liftIO getZonedTime
defaultAlbumArtist, defaultTrackTitle :: Text
defaultAlbumArtist = "Unknown Artist"
defaultTrackTitle  = "Unknown Track"

getSanitize :: MonadIO m => m (InputState -> InputState)
getSanitize = do
    defAlbTtl <- defaultAlbumTitle
    return $ \(state@InputState {..}) -> state
        { albumInfo = sanitizeItem defAlbTtl defaultAlbumArtist albumInfo
        , trackInfos = map (sanitizeItem defaultTrackTitle "") trackInfos
        }
  where
    sanitizeItem defTitle defFrom ItemInfo {..} = 
        let title' = sanitizeEntity defTitle title
            from' = sanitizeEntity defFrom from
        in ItemInfo title' from'
    sanitizeEntity dft val = if null val then dft else strip val

mkDirName :: ItemInfo -> FilePath
mkDirName ItemInfo {..} = fromText title </> fromText from

optionsParser :: Parser (Maybe FilePath)
optionsParser = optional $ optPath "work-dir" 'd' "Working directory"