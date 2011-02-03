module Skype.Entry where

import Data.DateTime
import Data.List
import Data.Word
import Data.ByteString

data SkypeEntry = SEntry {
    recSize :: Word32,
    sessionId :: Word32,
    msgId :: Int,
    timeStamp :: DateTime,
    senderId :: ByteString,
    senderName :: ByteString,
    recipientId :: ByteString,
    recipientName :: ByteString,
    members :: [ByteString],
    message :: ByteString,
    records :: [RawRecord]
} | IncompleteEntry { parseErrorMsg :: String } deriving Show

data RawRecord = IRecord {
                    recType :: RecordType,
                    intVal :: Word64 } |
                 TRecord { recType :: RecordType,
                    txtVal :: ByteString } |
                 BRecord


instance Show RawRecord where
    show (IRecord rType _) = show rType
    show (TRecord rType _) = show rType
    show BRecord = "BRecord"

data RecordType = 
    VoicemailFile | 
    Call | 
    Summary | 
    Language | 
    Country | 
    City | 
    File | 
    Peek | 
    Email | 
    URL | 
    Description | 
    Phone | 
    Type | 
    User | 
    Session | 
    Members | 
    Name | 
    Sender | 
    Recipient | 
    Message | 
    Member |
    Number | 
    Screenname | 
    Fullname | 
    LogBy |
    Special |
    Unknown { code :: Word64 } deriving (Show)

defaultTime = fromGregorian' 1970 1 1

makeSEntry = SEntry 0 0 0 defaultTime empty empty empty empty [] empty []

data SkypeChat = SChat {
    messages :: [SkypeEntry]
}
