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

import Control.Arrow

import Debug.Trace

newtype SortedSkypeEntry = SortedSkypeEntry { entries :: [SkypeEntry] }

type FolderName = ByteString

newtype ExportChat = ExportChat { export :: ( FolderName, (Int, SortedSkypeEntry) ) }

getFolderName = fst . export

getEntries = entries . snd . snd . export

getUsernameSize = fst . snd . export

sortEntries :: [SkypeEntry] -> (Int,SortedSkypeEntry)
sortEntries = second (SortedSkypeEntry . DS.toList) . DL.foldl go (0,DS.empty)
  where
    go (current,uniq) entry = (
                                max (DB8.length $ senderId entry) current, 
                                DS.insert entry uniq
                              )

exportChat :: String -> SkypeChat -> ExportChat
exportChat srcUser chat = ExportChat (folderName, (maxLen, sortedEntries))
    where
        (maxLen,sortedEntries) = sortEntries . messages $ chat
        firstEntry = DL.head . entries $ sortedEntries
        srcUser' = DB8.pack srcUser
        uL = userL chat
        uR = userR chat
        folderName | srcUser' == uL = uR
                 | srcUser' == uR = uL
                 | otherwise = srcUser'


separator = DB8.pack " : "
space = DB8.head $ DB8.pack " "

exportChats :: String -> String -> [SkypeChat] -> IO ()
exportChats folder username = mapM_ ( go . exportChat username )
    where 
        go chat = do
            mkdirs $ splitDirectories folder'
            bracket (openFile file' WriteMode)
                    hClose
                    (\handle -> hSetBinaryMode handle True >> doExport handle)
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
                usernameSize = getUsernameSize chat
                pad l str | len' == l = str
                          | otherwise = DB8.concat [str, DB8.replicate ( l - len') space]
                  where
                    len' = DB8.length str
                doExport handle =
                    mapM_ (writeEntry handle) $ getEntries chat
                writeEntry handle chatEntry | DB8.length content == 0 = return ()
                                            | otherwise = do
                                                let username = pad usernameSize $ senderId chatEntry
                                                let timestamp = formatTime defaultTimeLocale " %d-%m-%Y %H:%M:%S" . 
                                                                timeStamp $ chatEntry
                                                -- ugly formatting goes here
                                                S.hPutStr handle username
                                                S.hPutStr handle separator 
                                                S.hPutStr handle $ DB8.pack timestamp 
                                                S.hPutStr handle separator
                                                S.hPutStrLn handle content
                    where
                      content = message chatEntry
