module Skype.Entry where

import Data.DateTime
import Data.List
import Data.Word
import Data.ByteString

data SkypeEntry = SEntry {
    recSize :: Word32,
    sessionId :: Word32,
    msgId :: Word32,
    timeStamp :: DateTime,
    senderId :: ByteString,
    members :: [ByteString],
    message :: ByteString,
    records :: [RawRecord]
} | IncompleteEntry { parseErrorMsg :: String } deriving Show

data RawRecord = IRecord {
                    recType :: RecordType,
                    intVal :: Word64 } |
                 TRecord { recType :: RecordType,
                    txtVal :: ByteString } |
                 BRecord { recType :: RecordType,
                     blobVal :: ByteString, 
                     bRecSize :: Int} |
                 URecord { recType :: RecordType,
                     content :: ByteString }


instance Show RawRecord where
    show (IRecord rType val) = "I : " ++ show rType ++  " : " ++ show val
    show (TRecord rType val) = "T : " ++ show rType ++ " : " ++ show val
    show (BRecord rType _ _) = "B : " ++ show rType
    show (URecord rType _) = "U : " ++ show rType

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
    MsgId |
    Date |
    Unknown { code :: Word64 } deriving (Show)

defaultTime = fromGregorian' 1970 1 1

makeSEntry ::  SkypeEntry
makeSEntry = SEntry 0 0 0 defaultTime empty [] empty []

data SkypeChat = SChat {
    messages :: [SkypeEntry]
}
