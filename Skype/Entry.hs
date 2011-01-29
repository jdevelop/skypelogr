module Skype.Entry where

import Data.DateTime
import Data.List
import Data.Word
import Data.ByteString

data SkypeEntry = SEntry {
    sessionId :: Word32,
    msgId :: Int,
    timeStamp :: DateTime,
    senderId :: ByteString,
    senderName :: ByteString,
    recipientId :: ByteString,
    recipientName :: ByteString,
    members :: [ByteString],
    message :: ByteString
} | IncompleteEntry { parseErrorMsg :: String } deriving Show

defaultTime = fromGregorian' 1970 1 1

makeSEntry = SEntry 0 0 defaultTime empty empty empty empty [] empty


data SkypeChat = SChat {
    messages :: [SkypeEntry]
}
