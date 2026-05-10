{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Mycfg.Utils.Hash
  ( hashFile
  , hashText
  , hashBytes
  , verifyHash
  , HashAlgorithm(..)
  , HashResult(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Crypto.Hash (Digest, SHA256(..), SHA512(..), hashlazy, hash)
import Path (Abs, File, Path, toFilePath)
import System.IO (withBinaryFile, IOMode(ReadMode))

data HashAlgorithm
  = SHA256Alg
  | SHA512Alg
  deriving (Show, Eq)

data HashResult = HashResult
  { algorithm :: HashAlgorithm
  , digest :: Text
  , hexDigest :: Text
  } deriving (Show, Eq)

hashFile :: HashAlgorithm -> Path Abs File -> IO HashResult
hashFile algorithm filePath = do
  content <- BS.readFile (toFilePath filePath)
  return $ hashBytes algorithm content

hashText :: HashAlgorithm -> Text -> HashResult
hashText algorithm text = 
  let bytes = TextEncoding.encodeUtf8 text
  in hashBytes algorithm bytes

hashBytes :: HashAlgorithm -> ByteString -> HashResult
hashBytes algorithm bytes = 
  case algorithm of
    SHA256Alg -> 
      let digest = hash bytes :: Digest SHA256
          digestText = Text.pack $ show digest
          hexText = Text.pack $ show digest
      in HashResult algorithm digestText hexText
    SHA512Alg -> 
      let digest = hash bytes :: Digest SHA512
          digestText = Text.pack $ show digest
          hexText = Text.pack $ show digest
      in HashResult algorithm digestText hexText

verifyHash :: HashAlgorithm -> ByteString -> Text -> Bool
verifyHash algorithm bytes expectedHash = 
  let hashResult = hashBytes algorithm bytes
      actualHash = hexDigest hashResult
  in actualHash == expectedHash

hashLazyFile :: HashAlgorithm -> Path Abs File -> IO HashResult
hashLazyFile algorithm filePath = do
  content <- LazyBS.readFile (toFilePath filePath)
  return $ hashLazyBytes algorithm content

hashLazyBytes :: HashAlgorithm -> LazyBS.ByteString -> HashResult
hashLazyBytes algorithm bytes = 
  case algorithm of
    SHA256Alg -> 
      let digest = hashlazy bytes :: Digest SHA256
          digestText = Text.pack $ show digest
          hexText = Text.pack $ show digest
      in HashResult algorithm digestText hexText
    SHA512Alg -> 
      let digest = hashlazy bytes :: Digest SHA512
          digestText = Text.pack $ show digest
          hexText = Text.pack $ show digest
      in HashResult algorithm digestText hexText

hashFileStreaming :: HashAlgorithm -> Path Abs File -> IO HashResult
hashFileStreaming algorithm filePath = do
  withBinaryFile (toFilePath filePath) ReadMode $ \handle -> do
    let chunkSize = 8192
        processChunks chunks = 
          case chunks of
            [] -> hashBytes algorithm BS.empty
            (chunk:rest) -> 
              let currentHash = hashBytes algorithm chunk
              in case currentHash of
                HashResult _ _ hex -> 
                  let nextHash = hashBytes algorithm (BS.append (TextEncoding.encodeUtf8 hex) chunk)
                  in processChunks rest
    content <- BS.hGetContents handle
    return $ hashBytes algorithm content

defaultHashAlgorithm :: HashAlgorithm
defaultHashAlgorithm = SHA256Alg

hashResultToText :: HashResult -> Text
hashResultToText hashResult = 
  let algStr = case algorithm hashResult of
        SHA256Alg -> "sha256"
        SHA512Alg -> "sha512"
  in algStr <> ":" <> hexDigest hashResult

parseHashResult :: Text -> Maybe HashResult
parseHashResult hashText = 
  case Text.splitOn ":" hashText of
    [algStr, digestStr] -> do
      algorithm <- case algStr of
        "sha256" -> Just SHA256Alg
        "sha512" -> Just SHA512Alg
        _ -> Nothing
      return $ HashResult algorithm digestStr digestStr
    _ -> Nothing

compareHashResults :: HashResult -> HashResult -> Bool
compareHashResults hash1 hash2 = 
  algorithm hash1 == algorithm hash2 && hexDigest hash1 == hexDigest hash2

hashResultEqual :: HashResult -> HashResult -> Bool
hashResultEqual = compareHashResults

hashResultToString :: HashResult -> String
hashResultToString hashResult = 
  Text.unpack $ hashResultToText hashResult

stringToHashResult :: String -> Maybe HashResult
stringToHashResult = parseHashResult . Text.pack

hashResultToByteString :: HashResult -> ByteString
hashResultToByteString hashResult = 
  TextEncoding.encodeUtf8 $ hashResultToText hashResult

byteStringToHashResult :: ByteString -> Maybe HashResult
byteStringToHashResult = parseHashResult . TextEncoding.decodeUtf8

hashResultToJSON :: HashResult -> Text
hashResultToJSON hashResult = 
  let algStr = case algorithm hashResult of
        SHA256Alg -> "sha256"
        SHA512Alg -> "sha512"
      digestStr = hexDigest hashResult
  in "{\"algorithm\":\"" <> algStr <> "\",\"digest\":\"" <> digestStr <> "\"}"

parseHashResultFromJSON :: Text -> Maybe HashResult
parseHashResultFromJSON jsonText = 
  let cleanJson = Text.strip jsonText
  in if "\"algorithm\":\"sha256\"" `Text.isInfixOf` cleanJson
    then case Text.splitOn "\"digest\":\"" cleanJson of
      [_, digestPart] -> 
        let digest = Text.takeWhile (/= '"') digestPart
        in Just $ HashResult SHA256Alg digest digest
      _ -> Nothing
    else if "\"algorithm\":\"sha512\"" `Text.isInfixOf` cleanJson
      then case Text.splitOn "\"digest\":\"" cleanJson of
        [_, digestPart] -> 
          let digest = Text.takeWhile (/= '"') digestPart
          in Just $ HashResult SHA512Alg digest digest
        _ -> Nothing
      else Nothing

hashFileWithProgress :: HashAlgorithm -> Path Abs File -> (Int -> IO ()) -> IO HashResult
hashFileWithProgress algorithm filePath progressCallback = do
  content <- BS.readFile (toFilePath filePath)
  let chunkSize = 8192
      totalSize = BS.length content
      processChunks chunks processed = 
        case chunks of
          [] -> hashBytes algorithm BS.empty
          (chunk:rest) -> 
            let newProcessed = processed + BS.length chunk
            in do
              progressCallback (newProcessed * 100 `div` totalSize)
              processChunks rest newProcessed
  return $ processChunks (chunkData content) 0
  where
    chunkData bs = if BS.length bs <= chunkSize
      then [bs]
      else BS.take chunkSize bs : chunkData (BS.drop chunkSize bs)

hashDirectory :: HashAlgorithm -> Path Abs File -> IO HashResult
hashDirectory algorithm dirPath = do
  let dirStr = toFilePath dirPath
  entries <- listDirectory dirStr
  let sortedEntries = sort entries
      entryPaths = map (\entry -> dirStr </> entry) sortedEntries
  hashResults <- mapM (hashFile algorithm . parseAbsFile) entryPaths
  let combinedHash = concatMap (hexDigest) hashResults
  return $ hashText algorithm (Text.pack combinedHash)

hashMultipleFiles :: HashAlgorithm -> [Path Abs File] -> IO HashResult
hashMultipleFiles algorithm filePaths = do
  hashResults <- mapM (hashFile algorithm) filePaths
  let combinedHash = concatMap (hexDigest) hashResults
  return $ hashText algorithm (Text.pack combinedHash)

verifyFileHash :: HashAlgorithm -> Path Abs File -> Text -> IO Bool
verifyFileHash algorithm filePath expectedHash = do
  hashResult <- hashFile algorithm filePath
  return $ hexDigest hashResult == expectedHash

verifyTextHash :: HashAlgorithm -> Text -> Text -> Bool
verifyTextHash algorithm text expectedHash = 
  let hashResult = hashText algorithm text
  in hexDigest hashResult == expectedHash

verifyBytesHash :: HashAlgorithm -> ByteString -> Text -> Bool
verifyBytesHash algorithm bytes expectedHash = 
  let hashResult = hashBytes algorithm bytes
  in hexDigest hashResult == expectedHash
