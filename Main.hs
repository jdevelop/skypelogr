{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, DoAndIfThenElse #-}
module Main where

import Skype.Entry
import Skype.LogParser
import Skype.DBBLogParser
import Skype.SQLiteLogParser
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
import Data.ByteString.Lazy as LS
import qualified Data.Binary as DB

import Data.FileEmbed
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Crypto.Types.PubKey.RSA
import Codec.Crypto.RSA

import Licenser.License
import Licenser.Validate

licenses = DB.decode . LS.fromChunks . (:[]) $ $(embedFile "skypelogr.priv") :: [PrivateKey]

data Settings = Settings { 
  exportFolder :: FilePath, 
  skypeFolder :: Maybe FilePath, 
  registrationData :: Maybe TLicenseFile
  } 

defaultSettings = Settings "" Nothing Nothing

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

findUsers ::  FilePath -> IO [(FilePath, [SkypeEntry])]
findUsers root = 
  DL.filter (not . DL.null . snd) <$> (getDirectoryContents root >>= mapM diveInto . DL.filter removeSpecial)
  where
    diveInto src = let path = root </> src
                   in liftM ((,) path) $  processEntry root path
    findChatlogs folder = DL.concat <$> (getDirectoryContents folder >>= mapM (processEntry folder) . DL.filter removeSpecial)
    removeSpecial src = not $ src `DL.elem` [".",".."]
    processEntry parent entry = do
      fileExists <- doesFileExist path
      if (fileExists) 
        then goWithFile path
        else do
          dirExists <- doesDirectoryExist path
          if dirExists 
            then findChatlogs path
            else return []
      where
        path = parent </> entry
    chatPredicate x = x =~ "chat(msg)?\\d+.dbb$"
    dbPredicate x = x =~ "main.db$"
    goWithFile :: FilePath -> IO [SkypeEntry]
    goWithFile path | chatPredicate path = parseSkypeLog <$> S.readFile path
                    | dbPredicate path = return (parseSkypeLog (SQLFile path))
                    | otherwise = return []

main = getArgs >>= parseCmdLine >>= prepareEnv >>= fillLicense >>= go
  where
    go (Settings _ Nothing _ ) = Prelude.putStrLn "No Skype folders found"
    go (Settings targetFolder (Just skypeFolder) lcsz) = do
      entries <- findUsers skypeFolder
      if DL.null entries
        then Prelude.putStrLn "No Skype records found"
        else mapM_ (uncurry (
                      execute targetFolder (
                        maybe False (flip validateLicence licenses) lcsz
                      )
                    )
                  ) entries
    fillLicense s = do
      licenseFile <- liftM (</> "license") getCurrentDirectory
      haveLicenseFile <- doesFileExist licenseFile
      if (haveLicenseFile) 
      then 
        do
          license <- LS.readFile licenseFile
          return $ s { registrationData = Just ( DB.decode license ) }
      else
        return s { registrationData = Nothing }
    prepareEnv x@(Settings targetFolder Nothing _) = do
      folder <- getSkypeFolder
      return x { skypeFolder = folder }
    prepareEnv x = return x
    execute targetFolder full folder entries = do
      let username = DL.last . splitDirectories $ folder
      Prelude.putStrLn $ "Processing folder " ++ folder
      Prelude.putStrLn $ "Total messages found: " ++ show (DL.length entries)
      let chats = aggregateLogs [if full then entries else takeSafe 20 entries]
      let totals = DL.sum $ DL.map ( DL.length . messages ) chats
      Prelude.putStrLn $ "Exported sessions " ++ show (DL.length chats)
      Prelude.putStrLn $ "Exported messages: " ++ show totals
      Prelude.putStrLn $ "Exporting chats for " ++ username
      exportChats targetFolder username chats
      Prelude.putStrLn $ "Done" 
    takeSafe _ [] = []
    takeSafe n xs = DL.take n xs
