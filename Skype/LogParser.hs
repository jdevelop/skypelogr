module Skype.LogParser where

import Data.ByteString.Lazy as L
import Data.ByteString as S

import Data.Attoparsec as AP

import Data.Binary.Get

import Data.Bits

import Data.Maybe

import Data.List as DL

import Data.Map as DM

import Data.Word

import Control.Monad

import Skype.Entry

class LogParser s where
    parseSkypeLog :: s -> [SkypeEntry]

extractResults = ((either (const []) id . AP.eitherResult) .) . (flip AP.feed S.empty .) . AP.parse

instance LogParser S.ByteString where
    parseSkypeLog = extractResults (many parsecLogParser)

parsecLogParser ::  Parser SkypeEntry
parsecLogParser = do
    string $ S.pack [0x6C,0x33,0x33,0x6C]
    recSz <- read4Bytes
    session <- read4Bytes
    AP.take 5
    content <- AP.take $ fromIntegral recSz
    AP.skipWhile ( == 0x00 )
    return . extractResult $ parse (parseLogContent recSz session) content
    where
        extractResult ( Fail _ _ msg ) = IncompleteEntry msg
        extractResult ( Partial f ) = extractResult $ f S.empty
        extractResult ( Done _ r ) = r

parseLogContent :: Word32 -> Word32 -> Parser SkypeEntry
parseLogContent recSz skypeSession = do
    let skypeEntry = makeSEntry { sessionId = skypeSession, recSize=recSz }
    records <- try $ many parseRecord
    return $ DL.foldl mkSkypeEntry skypeEntry records
    where
        mkSkypeEntry rec fld = let fields = fld:rawRecs rec
                               in rec { rawRecs = fields }

parseRecord :: Parser RawRecord
parseRecord = do
    try skipGarbage
    try skipDelimiter
    recType <- liftM deriveType readNumber
    value <- AP.takeWhile ( /= 0x03 )
    return $ Record recType value

deriveType :: Word64 -> RecordType
--deriveType 15 = VoicemailFile
--deriveType 16 = Call
--deriveType 20 = Summary
--deriveType 36 = Language
--deriveType 40 = Country
--deriveType 48 = City
--deriveType 51 = File
deriveType 55 = Message
--deriveType 64 = Email
--deriveType 68 = URL
--deriveType 72 = Description
--deriveType 116 = Country
--deriveType 184 = Phone
--deriveType 296 = Type
--deriveType 404 = User
--deriveType 408 = User
deriveType 440 = Session
deriveType 456 = Members
deriveType 460 = Members
deriveType 468 = User
deriveType 472 = Name
--deriveType 480 = Session
deriveType 488 = Sender
deriveType 492 = Sender
deriveType 500 = Recipient
deriveType 508 = Message
--deriveType 584 = Session
deriveType 588 = Member
deriveType 828 = User
deriveType 840 = User
deriveType 868 = Number
deriveType 920 = Screenname
deriveType 924 = Fullname
deriveType 3160 = LogBy
deriveType num | num > 1000000000 = Special
deriveType num = Unknown num

skipGarbage = AP.skipWhile ( /= 0x03 )

skipDelimiter = AP.skipWhile ( == 0x03 )

read4Bytes = liftM ( runGet getWord32le . (L.fromChunks . (:[])) ) $ AP.take 4

readNumber ::  Parser Word64
readNumber = do
    buf <- AP.takeWhile ( \c -> c .&. 0x80 == 0x80 )
    rem <- liftM S.head $ AP.take 1
    return $ DL.foldl makeNum 0 $ DL.zip (S.unpack $ buf `S.snoc` rem) [0,7..]
    where 
        makeNum :: Word64 -> (Word8, Int) -> Word64
        makeNum acc (dig, bits) = acc .|. ( ( (fromIntegral dig) .&. 0x7f) `shift` bits )
