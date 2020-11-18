module DiscHandling.Inspection where

import Prelude hiding (head, lines, null, tail)
import Control.Exception.Base (catchJust, fromException, SomeException)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Char (isDigit)
import Data.Text (lines, null, pack, Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Vector (empty, head, snoc, tail, Vector)
import Text.Regex.Applicative.Text ((<|>), anySym, few, match, optional, psym, RE', string, sym)
import Turtle (echo, err, ExitCode (..), fold, Fold (..), Line, lineToText, select, Shell, unsafeTextToLine)
import Turtle.Bytes (inshell, strict)

import UI.Types

readDiscInfo :: MonadIO m => m (Maybe (Shell Line)) 
readDiscInfo = do
    echo "Reading disc info..."
    liftIO $ catchJust toExitCode 
        (Just 
            . select
            . (unsafeTextToLine <$>)
            . filter (not . null) 
            . lines 
            . decodeUtf8With utf8Error 
            <$> strict (inshell discInfoCmd mempty)

            {- 
            -- return 
            -- $ Just
            -- $ (unsafeTextToLine <$>)
            -- $ concat
            -- $ filter null . lines . decodeUtf8With utf8Error 
            -- <$> inshell discInfoCmd mempty
            -}
            )
        (\(ExitFailure e) -> do
            err $ unsafeTextToLine $ "Error reading disc info: " <> pack (show e)
            return Nothing
            )
  where
    utf8Error _ = \case
        Nothing -> Nothing
        Just _ -> Just '?'
    discInfoCmd = "cdda2wav dev=/dev/cdrom -gui -cddb 1 -no-textfile -no-infofile -info-only -alltracks -verbose-level=titles out-fd=1 cddbp-server=gnudb.gnudb.org"

toExitCode :: SomeException -> Maybe ExitCode
toExitCode = fromException

parseDiscInfo :: MonadIO m => Maybe (Shell Line) -> m InputState
parseDiscInfo = \case
    Nothing -> do
        err $ "No disc data"
        return emptyInputState
    Just s -> 
        let f = Fold step init extract
            step vec line = 
                let t = lineToText line
                    m = match parseLine t
                in maybe vec (snoc vec) m
            init = empty
            extract = \v -> InputState (head v) (tail v) id 0
        in fold s f
                    
parseLine :: RE' ItemInfo
parseLine = ItemInfo <$> (parsePfx *> title) <*> from
  where
    parsePfx = string "Album " <|> (sym 'T' *> dgt *> dgt *> string ": ")
    dgt = psym isDigit
    title = pack <$> (string "title" *> optional (sym ':') *> string " '" *> few (anySym)) <* sym '\''
    from = pack <$> (string " from '" *> few (anySym)) <* sym '\''
