module Skype.Entry where

import Data.DateTime
import Data.List as DL
import Data.Word
import Data.ByteString
import Data.ByteString.UTF8 as U

data SkypeEntry = SEntry {
    recSize :: Word32,
    sessionId :: ByteString,
    timeStamp :: DateTime,
    senderId :: ByteString,
    members :: [ByteString],
    message :: ByteString,
    records :: [RawRecord]
} | IncompleteEntry { parseErrorMsg :: String } deriving Show

instance Eq (SkypeEntry) where
    a == b = sessionId a == sessionId b && 
             timeStamp a == timeStamp b &&
             senderId a == senderId b

instance Ord (SkypeEntry) where
    a `compare` b | timeStamp a < timeStamp b = LT
                  | timeStamp a > timeStamp b = GT
                  | otherwise                 = EQ

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
makeSEntry = SEntry 0 empty defaultTime empty [] empty []

data SkypeChat = SChat {
    userL, userR :: ByteString,
    messages :: [SkypeEntry]
}

instance Show SkypeChat where
    show s = U.toString (userL s) ++ " : " ++ U.toString ( userR s ) ++ " (" ++ show ( DL.length (messages s) ) ++ ")"
