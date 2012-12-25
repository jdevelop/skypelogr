module Skype.SQLiteLogParser where

import Skype.LogParser
import Skype.Entry
import Skype.Util

import System.IO.Unsafe (unsafePerformIO)

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Applicative ((<$>))

import qualified Data.ByteString.Char8 as U8

import Debug.Trace

newtype SQLFile = SQLFile { getFileName :: String }

type Rows = [[SqlValue]]

instance LogParser SQLFile where
  parseSkypeLog file = unsafePerformIO $ 
    connectSqlite3 (getFileName file) >>= 
    buildSkypeMessages 

emptyU8 = U8.pack ""

buildSkypeMessages :: Connection -> IO [SkypeEntry]
buildSkypeMessages dbh = 
  concatMap f <$> (quickQuery dbh "select chatname, author, timestamp, body_xml from messages order by chatname, timestamp" [])
  where
    f' (SqlNull:rest) = []
    f' ((SqlByteString chatname):
        (SqlByteString author):
        timestamp:
        r) = [(SEntry 0 chatname
                       (fromSeconds (fromIntegral (fromSql (timestamp) :: Integer))) 
                       author
                       [],
                       r)]
    f' xs = traceShow (concatMap show xs) []
    f xs = z <$> f' xs
    z (partEntry, ((SqlByteString v):_))  = partEntry v []
    z (partEntry, (SqlNull:_))            = partEntry emptyU8 []
    z (partEntry, s)                      = error $ show s
