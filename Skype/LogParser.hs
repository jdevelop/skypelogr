module Skype.LogParser (
    LogParser(parseSkypeLog)
) where

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
import Skype.Util

class LogParser s where
    parseSkypeLog :: s -> [SkypeEntry]

extractResults ::  Parser [a] -> S.ByteString -> [a]
extractResults = ((either (const []) id . AP.eitherResult) .) . (flip AP.feed S.empty .) . AP.parse

instance LogParser S.ByteString where
    parseSkypeLog = extractResults (many parsecLogParser)

headerSign' = [0x6C,0x33,0x33,0x6C]

parsecLogParser ::  Parser SkypeEntry
parsecLogParser = do
    skipUntilString headerSign'
    recSz <- read4Bytes
    AP.take 9
    content <- AP.take $ fromIntegral recSz - 9
    return . extractResult $ parse (parseLogContent recSz) content
    where
        extractResult ( Fail _ _ msg ) = IncompleteEntry msg
        extractResult ( Partial f ) = extractResult $ f S.empty
        extractResult ( Done _ r ) = r

skipUntilString :: [Word8] -> Parser ()
skipUntilString start = go start
    where
        go [] = return ()
        go (x:xs) = do
            cur <- AP.take 1
            handleChar x xs (S.head cur)
        handleChar x xs cur | x == cur = go xs
                            | cur == DL.head start = go $ DL.tail start
                            | otherwise = go start

parseLogContent :: Word32 -> Parser SkypeEntry
parseLogContent recSz = do
    let skypeEntry = makeSEntry { recSize=recSz }
    records <- try $ many parseRecord
    return $ DL.foldl mkSkypeEntry skypeEntry records
    where
        mkSkypeEntry rec (TRecord Message val) = rec { message = val }
        mkSkypeEntry rec (TRecord Sender val) = rec { senderId = val }
        mkSkypeEntry rec (TRecord Members val) = rec { members = splitRec val }
        mkSkypeEntry rec (IRecord Date val) = rec { timeStamp = fromSeconds val }
        mkSkypeEntry rec (TRecord Session val) = rec { sessionId = val }
        mkSkypeEntry rec fld = let recs = records rec 
                               in rec { records = fld:recs }
        splitRec = S.split 0x20


intMark = S.pack [0x00]
textMark = S.pack [0x03]
blobMark = S.pack [0x04]

parseRecord ::  Parser RawRecord
parseRecord = AP.take 1 >>= handleRecord 
    where
        handleRecord src | src == intMark = parseInt
                         | src == textMark = parseText
                         | src == blobMark =  parseBlob
                         | otherwise = parseUnknown

parseInt ::  Parser RawRecord
parseInt = do
    itemCode <- liftM deriveType readNumber
    itemValue <- readNumber
    return $ IRecord itemCode itemValue

parseText ::  Parser RawRecord
parseText = do
    itemCode <- liftM deriveType readNumber
    content <- AP.takeWhile ( /= 0x00 )
    AP.take 1
    return $ TRecord itemCode content

parseBlob ::  Parser RawRecord
parseBlob = do
    itemCode <- liftM deriveType readNumber
    itemSize <- readNumber
    content <- AP.take $ fromIntegral itemSize
    return $ BRecord itemCode content ( fromIntegral itemSize )

parseUnknown = do
    itemCode <- liftM deriveType readNumber
    content <- AP.takeWhile ( > 0x04 )
    return $ URecord itemCode content


deriveType :: Word64 -> RecordType
deriveType 15 = VoicemailFile
deriveType 16 = Call
deriveType 20 = Summary
deriveType 36 = Language
deriveType 40 = Country
deriveType 48 = City
deriveType 51 = File
deriveType 55 = Message
deriveType 508 = Message
deriveType 64 = Email
deriveType 68 = URL
deriveType 72 = Description
deriveType 116 = Country
deriveType 184 = Phone
deriveType 296 = Type
deriveType 404 = User
deriveType 408 = User
deriveType 440 = Session
deriveType 456 = Members
deriveType 460 = Members
deriveType 468 = User
deriveType 472 = Name
deriveType 480 = Session
deriveType 488 = Sender
deriveType 492 = Sender
deriveType 500 = Recipient
deriveType 584 = Session
deriveType 588 = Member
deriveType 565 = Date
deriveType 485 = Date
deriveType 828 = Sender
deriveType 840 = User
deriveType 868 = Number
deriveType 920 = Screenname
deriveType 924 = Fullname
deriveType 3160 = LogBy
deriveType num = Unknown num

skipGarbage = AP.skipWhile ( /= 0x03 )

skipDelimiter = AP.skipWhile ( == 0x03 )

read4Bytes = liftM ( runGet getWord32le . L.fromChunks . (:[]) ) $ AP.take 4
