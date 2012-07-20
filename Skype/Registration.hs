module Skype.Registration where

import Codec.Crypto.RSA
import Crypto.Types.PubKey.RSA
import Crypto.Random
import Control.Applicative((<$>))
import Control.Arrow
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as B8L
import qualified Data.List as DL
import qualified Data.Binary as DB

import System.Entropy

data TLicenseFile = LicenseFile {
  randomData :: B8.ByteString,
  keyId :: Int,
  signature :: B8.ByteString,
  encryptedData :: B8.ByteString
}

createLicense :: String -> String -> Int -> (PublicKey,PrivateKey) -> IO TLicenseFile
createLicense username email idx keyPair = do
  (second (arr (const signLicense)) . encryptLicense) <$> (newGenIO :: IO SystemRandom) >>= build
  where
    licenseData = B8L.fromChunks [B8.pack $ username ++ " :: " ++ email]
    encryptLicense randGen = encrypt randGen (fst keyPair) licenseData
    signLicense = sign (snd keyPair) licenseData
    makeStrict = B8.concat . B8L.toChunks
    build (encrypted, signature) = do
      bytes <- getEntropy 20
      return $ LicenseFile bytes idx 
        (makeStrict signature) 
        (makeStrict encrypted)

validateLicence :: TLicenseFile -> [PrivateKey] -> Bool
validateLicence (LicenseFile rndData idx curSign encr) keys = curSign == newSign
  where
    privKey = head $ drop idx keys
    rawData = decrypt privKey $ B8L.fromChunks [encr]
    newSign = B8.concat . B8L.toChunks $ sign privKey rawData

generateKeyPairs :: Int -> Int -> IO [(PublicKey,PrivateKey)]
generateKeyPairs bits numLicenses = do
  gen <- newGenIO :: (IO SystemRandom)
  return $ go gen numLicenses
  where
    go gen 0 = []
    go gen n = let (pub,priv,g) = generateKeyPair gen bits
               in (pub,priv) : go g (pred n)

instance DB.Binary TLicenseFile where
  get = 
    do
      rndData <- DB.get
      idx <- DB.get
      signData <- DB.get
      encrData <- DB.get
      return (LicenseFile rndData idx signData encrData)
  put (LicenseFile rndData idx signData encrData) = do
    DB.put rndData
    DB.put idx
    DB.put signData
    DB.put encrData

