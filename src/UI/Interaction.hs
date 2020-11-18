-- {-# LANGUAGE BlockArguments #-}

module UI.Interaction where

import Prelude hiding(getLine, putStr, putStrLn)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Loops (untilJust)
import Data.Text as Text (any, pack, take, toUpper, unpack, Text)
import Data.Text.IO (getLine, putStr, putStrLn)
import Turtle (echo, ExitCode (..), Line)
import System.IO (BufferMode (..), hGetBuffering, hSetBuffering, stdin, stdout)

getRetryConfirm, getContinueConfirm, getRipConfirm :: MonadIO m => m Bool
getRetryConfirm    = getConfirm "Retry?"
getContinueConfirm = do
    res <- getConfirm "Rip one more disk?"
    when res $
        liftIO $ putStrLn "==============================================="
    return res

getRipConfirm      = getConfirm "Disk will be ripped. OK?"

getConfirm :: MonadIO m => Text -> m Bool 
getConfirm msg = do
    ok <- liftIO $ do
        saveInBuf <- hGetBuffering stdin
        saveOutBuf <- hGetBuffering stdout
        hSetBuffering stdin LineBuffering
        hSetBuffering stdout NoBuffering
        putStr msg
        ok <- getLine
        putStrLn ""
        hSetBuffering stdout saveOutBuf
        hSetBuffering stdin saveInBuf
        return ok
    return $ Text.any (== 'Y') $ Text.toUpper $ Text.take 1 ok

retryProcess :: MonadIO m => m ExitCode -> m () 
retryProcess process = untilJust (do
    result <- process
    case result of 
        ExitSuccess -> return $ Just ()
        ExitFailure e -> do
            liftIO $ putStrLn $ "Error retrying: " <> pack (show e)
            ok <- getRetryConfirm
            return $ if ok then Nothing else Just ()
    )

promptDisc :: MonadIO m => m ()
promptDisc = void $ getConfirm "Place CD/DVD in device and press Enter"
