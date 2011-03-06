module Main where

import Skype.Entry
import Skype.LogAggregator
import Skype.LogParser
import Skype.LogExport
import Control.Monad
import Data.List as DL
import System.Directory
import System
import Text.Regex.PCRE
import System.FilePath
import Data.ByteString as S

listChatFiles :: FilePath -> IO [FilePath]
listChatFiles path = (DL.map ( path </> ) . DL.filter chatPredicate ) `fmap` getDirectoryContents path
    where
        chatPredicate x = x =~ "chat(msg)?\\d+.dbb"

main = do
    getArgs >>= go
    where
        go (skypeFolder:targetFolder:[]) = do
            let username = DL.last . splitDirectories $ skypeFolder
            Prelude.putStrLn $ "Processing folder " ++ skypeFolder
            files <- listChatFiles skypeFolder
            Prelude.putStrLn $ "History files found: " ++ (show $ DL.length files)
            chats <- (aggregateLogs . DL.map parseSkypeLog) `fmap` mapM S.readFile files
            let totals = DL.sum $ DL.map ( DL.length . messages ) chats
            Prelude.putStrLn $ "Sessions found: " ++ (show $ DL.length chats)
            Prelude.putStrLn $ "Messages found: " ++ (show totals)
            Prelude.putStrLn $ "Exporting chats for " ++ username ++ " to folder " ++ targetFolder
            exportChats targetFolder username chats
            Prelude.putStrLn $ "Done, results available under" ++ targetFolder
        go _ = Prelude.putStrLn "Usage: skypeexport <skype folder> <output folder>"
