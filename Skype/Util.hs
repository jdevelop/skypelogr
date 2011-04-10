module Skype.Util where

import Data.Attoparsec as AP
import Data.Word
import Control.Monad
import Data.Bits
import Data.ByteString as S
import Data.List as DL
import Text.Printf
import Data.Time
import Data.Time.Calendar as C

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

fromGregorian' :: Integral a => Integer -> Int -> Int -> a -> a -> a -> UTCTime
fromGregorian' year month day hours minutes seconds =
    UTCTime day' (secondsToDiffTime . fromIntegral $ seconds')
        where
            day'     = C.fromGregorian year month day
            seconds' = 3600 * hours + 60 * minutes + seconds

startGT ::  UTCTime
startGT = fromGregorian' 1970 1 1 0 0 0

startOfTimeMJD ::  Rational
startOfTimeMJD = toMJD startGT

fromUniversalTime ::  UniversalTime -> UTCTime
fromUniversalTime = localTimeToUTC utc . ut1ToLocalTime 0

fromMJD ::  Rational -> UTCTime
fromMJD = fromUniversalTime . ModJulianDate

toMJD ::  UTCTime -> Rational
toMJD = getModJulianDate . toUniversalTime

toUniversalTime ::  UTCTime -> UniversalTime
toUniversalTime = localTimeToUT1 0 . utcToLocalTime utc

fromSeconds ::  Integral a => a -> UTCTime
fromSeconds s =  fromMJD $ fromIntegral s / 86400 + startOfTimeMJD
