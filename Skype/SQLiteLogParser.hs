module Skype.SQLiteLogParser where

import Skype.LogParser
import Skype.Entry
import Skype.Util

import System.IO.Unsafe (unsafePerformIO)

import qualified Database.SQLite as SQL
import Control.Applicative ((<$>))

import qualified Data.ByteString.UTF8 as U8

newtype SQLFile = SQLFile { getFileName :: String }

type Rows = [[SQL.Row SQL.Value]]

instance LogParser SQLFile where
  parseSkypeLog file = unsafePerformIO $ 
    SQL.openReadonlyConnection (getFileName file) >>= 
    buildSkypeMessages 

emptyU8 = U8.fromString ""

buildSkypeMessages :: SQL.SQLiteHandle -> IO [SkypeEntry]
buildSkypeMessages dbh = 
  go <$> (SQL.execStatement dbh "select chatname,author,timestamp,body_xml from messages order by chatname, timestamp")
  where
    go (Left msg) = fail msg
    go (Right rows) = map f $ concat rows
      where
        f' (("chatname",SQL.Text chatname):
            ("author",SQL.Text author):
            ("timestamp",SQL.Int timestamp):
            r) = (SEntry 0 (U8.fromString chatname) 
                           (fromSeconds (fromIntegral timestamp)) 
                           (U8.fromString author)      
                           [],
                           r)
        f xs = let (partEntry, (item:_)) = f' xs
               in case item of
                 ("body_xml",SQL.Text v) -> partEntry (U8.fromString v) []
                 ("body_xml",SQL.Null)   -> partEntry emptyU8 []
        escape (_,SQL.Text v) = v
        escape (_,SQL.Null) = ""
        escape (_,SQL.Int v) = show v
