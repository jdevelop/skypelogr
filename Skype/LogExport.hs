module Skype.LogExport (
    exportChats
) where

import Skype.Entry
import Data.Set as DS
import Data.List as DL 
import Data.ByteString.Char8 as DB8
import Data.ByteString as S
import Data.Time
import Control.Monad
import Control.Monad.IfElse
import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import System.Locale

newtype SortedSkypeEntry = SortedSkypeEntry { entries :: [SkypeEntry] }

type FolderName = ByteString

newtype ExportChat = ExportChat { export :: ( FolderName, SortedSkypeEntry ) }

getFolderName = fst . export

getEntries = entries . snd . export

sortEntries :: [SkypeEntry] -> SortedSkypeEntry
sortEntries = SortedSkypeEntry . DS.toList . DS.fromList

exportChat :: String -> SkypeChat -> ExportChat
exportChat srcUser chat = ExportChat (folderName, sortedEntries)
    where
        sortedEntries = sortEntries . messages $ chat
        firstEntry = DL.head . entries $ sortedEntries
        srcUser' = DB8.pack srcUser
        uL = userL chat
        uR = userR chat
        folderName | srcUser' == uL = uR
                 | srcUser' == uR = uL
                 | otherwise = srcUser'


separator = DB8.pack " : "

exportChats :: String -> String -> [SkypeChat] -> IO ()
exportChats folder username = mapM_ ( go . exportChat username )
    where 
        go chat = do
            mkdirs $ splitDirectories folder'
            bracket (openFile file' WriteMode)
                    hClose
                    doExport
            where
                mkdirs [dir] = mkDir dir
                mkdirs (dir:new:dirs) = do
                    mkDir dir
                    mkdirs $ (dir </> new) : dirs
                mkDir dir = 
                    whenM ( liftM not $ doesDirectoryExist dir ) 
                        ( createDirectory dir )
                folder' = folder </> username </> DB8.unpack folderName
                file' = folder' </> firstEntryDateStr <.> "log"
                firstEntry = DL.head . getEntries $ chat
                firstEntryDateStr = formatTime defaultTimeLocale "%Y-%m-%d  %H-%M-%S" . timeStamp $ firstEntry
                folderName = getFolderName chat
                doExport handle =
                    mapM_ (writeEntry handle) $ getEntries chat
                writeEntry handle chatEntry = do
                    let username = senderId chatEntry
                    let content = message chatEntry
                    let timestamp = formatTime defaultTimeLocale " %d-%m-%Y %H:%M:%S" . timeStamp $ chatEntry
                    -- ugly formatting goes here
                    S.hPutStr handle username
                    S.hPutStr handle separator 
                    S.hPutStr handle $ DB8.pack timestamp 
                    S.hPutStr handle separator
                    S.hPutStrLn handle content
