module Skype.FolderResolver (
  getSkypeFolder
) where

import System.Directory
import System.FilePath
import Data.Maybe
import Control.Monad (liftM)

osDependentFolders = [
  "Application Data/Skype",
  "AppData/Roaming/Skype",
  ".Skype"
 ]

getSkypeFolder :: IO (Maybe FilePath)
getSkypeFolder = do
  home <- getHomeDirectory
  liftM (liftM fst . listToMaybe . filter snd ) $ mapM (f home) osDependentFolders
  where
    f home folder = do 
      let target = home </> folder
      doesExist <- doesDirectoryExist target
      return (target, doesExist)
