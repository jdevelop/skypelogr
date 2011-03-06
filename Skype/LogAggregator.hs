module LogAggregator (
        aggregateLogs
) where

import Skype.LogParser
import Skype.Entry
import Data.Map as DM
import Data.List as DL
import Data.Word
import Data.ByteString as S
import Data.Attoparsec as DA

type ChatsMap = DM.Map S.ByteString [SkypeEntry]

chatsMap :: ChatsMap
chatsMap = DM.empty

aggregateLogs :: [[SkypeEntry]] -> [SkypeChat]
aggregateLogs = extractChats . DL.foldr allocateEntry chatsMap . DL.concat
    where
        allocateEntry :: SkypeEntry -> ChatsMap -> ChatsMap
        allocateEntry entry acc = DM.alter ( updMap entry ) ( sessionId entry ) acc
        updMap entry Nothing = Just [entry]
        updMap entry (Just entries) = Just $ entry : entries
        extractChats :: ChatsMap -> [SkypeChat]
        extractChats = DM.foldWithKey go []
        go key value arr = let ( userA, userB ) = extractResult $ DA.parse parseChatSession key
                           in SChat userA userB value : arr
        extractResult (Done _ r) = r
        extractResult _ = ( S.empty, S.empty )

parseChatSession :: Parser (S.ByteString, S.ByteString)
parseChatSession = do
    word8 0x23
    userA <- DA.takeWhile ( /= 0x2f )
    word8 0x2f
    word8 0x24
    userB <- DA.takeWhile ( /= 0x3b )
    return (userA, userB)
