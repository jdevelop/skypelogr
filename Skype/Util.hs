module Skype.Util where

import Data.Attoparsec as AP
import Data.Word
import Control.Monad
import Data.Bits
import Data.ByteString as S
import Data.List as DL
import Text.Printf
import Data.Time
import Data.Time.Clock.POSIX

readNumber ::  Parser Word64
readNumber = do
    buf <- AP.takeWhile ( \c -> c .&. 0x80 > 0 )
    rem <- liftM S.head $ AP.take 1
    return $ DL.foldl makeNum 0 $ DL.zip (S.unpack $ buf `S.snoc` rem) [0,7..]
    where 
        makeNum :: Word64 -> (Word8, Int) -> Word64
        makeNum acc (dig, bits) = acc .|. ( ( fromIntegral dig .&. 0x7f) `shift` bits )

convertToBytes :: String -> [Word8]
convertToBytes [] = []
convertToBytes (x:y:xs) = read ['0','x',x,y] : convertToBytes xs

parseNumStr :: String -> Result Word64
parseNumStr = parseNum . convertToBytes

parseNum :: [Word8] -> Result Word64
parseNum = parse readNumber . S.pack 

showNum :: Word64 -> [Word8]
showNum item | next == 0 = [cur]
             | otherwise = (cur .|. 0x80) : showNum next
    where
         cur = fromIntegral item .&. 0x7f
         next = item `shiftR` 7

printNum :: Word64 -> String
printNum = DL.intercalate "," . DL.map (printf "%0x") . showNum

fromSeconds :: Word64 -> UTCTime
fromSeconds = posixSecondsToUTCTime . realToFrac
