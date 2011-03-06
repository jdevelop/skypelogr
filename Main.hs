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
listChatFiles path = DL.filter chatPredicate `fmap` getDirectoryContents path
    where
        chatPredicate x = x =~ "chat(msg)?\\d+.dbb"

main = do
    getArgs >>= go
    where
        go (skypeFolder:targetFolder:[]) = do
            let username = takeFileName skypeFolder
            listChatFiles skypeFolder >>= 
                mapM S.readFile >>= 
                exportChats username targetFolder . aggregateLogs . DL.map parseSkypeLog
        go _ = Prelude.putStrLn "Usage: skypeexport <skype folder> <output folder>"
