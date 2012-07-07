module Main where

import Skype.Entry
import Skype.LogParser
import Skype.DBBLogParser
import Skype.LogAggregator
import Skype.LogExport
import Skype.FolderResolver

import Control.Monad (liftM,foldM)
import Control.Applicative ((<$>))
import Data.List as DL
import Data.Maybe
import System.Directory
import System
import System.FilePath
import System.Console.GetOpt
import Text.Regex.PCRE
import Text.Printf
import Data.ByteString as S

data Settings = Settings { exportFolder :: FilePath, skypeFolder :: Maybe FilePath } deriving Show

defaultSettings = Settings "" Nothing

options :: [OptDescr (Settings -> IO Settings)]
options = 
  [ Option ['s']  ["source"] (ReqArg checkAndSetInput "")  "Skype data folder",
    Option ['d']  ["destination"] (ReqArg checkAndSetOutput "") "Skype destination folder"
  ]
  where
    checkAndSetInput dir opts = do
      exists <- doesDirectoryExist dir
      if (exists) 
        then return $ opts {skypeFolder = Just dir}
        else ioError $ userError (printf "Source folder %s does not exist" dir)
    checkAndSetOutput dir opts = do
      exists <- doesDirectoryExist dir
      if (exists) 
        then return $ opts {exportFolder = dir}
        else ioError $ userError (printf "Export folder %s does not exist" dir)

parseCmdLine argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> foldM ( flip id ) defaultSettings o
    (_,_,errs) -> ioError (userError (DL.concat errs ++ usageInfo header options))
  where 
    header = "Usage: ic [OPTION...] files..."

listChatFiles :: FilePath -> IO [FilePath]
listChatFiles path = (DL.map ( path </> ) . DL.filter chatPredicate ) `fmap` getDirectoryContents path
    where
        chatPredicate x = x =~ "chat(msg)?\\d+.dbb"

findUsers root = 
  DL.filter (not . DL.null . snd) <$> (getDirectoryContents root >>= mapM diveInto . DL.filter removeSpecial)
  where
    diveInto src = let path = root </> src
                   in liftM ((,) path) $  processEntry root path
    findChatlogs folder = DL.concat <$> (getDirectoryContents folder >>= mapM (processEntry folder) . DL.filter removeSpecial)
    removeSpecial src = not $ src `DL.elem` [".",".."]
    processEntry parent entry = do
      fileExists <- doesFileExist path
      if (fileExists && (chatPredicate entry || dbPredicate entry))
        then return [path]
        else do
          dirExists <- doesDirectoryExist path
          if dirExists 
            then findChatlogs path
            else return []
      where
        path = parent </> entry
    chatPredicate x = x =~ "chat(msg)?\\d+.dbb"
    dbPredicate x = x =~ "main.db"


main = getArgs >>= parseCmdLine >>= prepareEnv >>= go
  where
    go (Settings targetFolder (Just skypeFolder)) = do
      mappings <- findUsers skypeFolder
      if DL.null mappings 
        then Prelude.putStrLn "No Skype records found"
        else mapM_ (uncurry (execute targetFolder)) mappings
    prepareEnv x@(Settings targetFolder Nothing) = do
      folder <- getSkypeFolder
      return x { skypeFolder = folder }
    prepareEnv x = return x
    execute targetFolder folder files = do
      let username = DL.last . splitDirectories $ folder
      Prelude.putStrLn $ "Processing folder " ++ folder
      Prelude.putStrLn $ "History files found: " ++ show (DL.length files)
      chats <- (aggregateLogs . DL.map parseSkypeLog) `fmap` mapM S.readFile files
      let totals = DL.sum $ DL.map ( DL.length . messages ) chats
      Prelude.putStrLn $ "Sessions found: " ++ show (DL.length chats)
      Prelude.putStrLn $ "Messages found: " ++ show totals
      Prelude.putStrLn $ "Exporting chats for " ++ username ++ " to folder " ++ targetFolder
      exportChats targetFolder username chats
      Prelude.putStrLn $ "Done, results available under " ++ targetFolder
