-- |
-- Module      : Algebirc.Ordering.Fingerprint
-- Description : SHA-256 fingerprinting for file identity and ordering
-- License     : MIT
--
-- Each encoded module gets a unique fingerprint derived from its content.
-- This fingerprint determines execution order via mathematical priority.

module Algebirc.Ordering.Fingerprint
  ( -- * Fingerprint Generation
    FileFingerprint(..)
  , computeFingerprint
  , computeBlockFingerprint
    -- * Hash Utilities
  , hashToInteger
  , hashToFieldElement
  ) where

import Algebirc.Core.Types
import Algebirc.Obfuscation.Encoder (EncodedBlock(..), EncodedModule(..))
import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl')
import Data.Word (Word8)

-- ============================================================
-- Types
-- ============================================================

-- | A file fingerprint: SHA-256 hash + derived priority value.
data FileFingerprint = FileFingerprint
  { fpName       :: !String            -- ^ Module/file name
  , fpHash       :: !BS.ByteString     -- ^ Raw SHA-256 hash (32 bytes)
  , fpHashHex    :: !String            -- ^ Hex string
  , fpPriority   :: !Integer           -- ^ Mathematical priority value
  } deriving (Show, Eq)

instance Ord FileFingerprint where
  compare a b = compare (fpPriority a) (fpPriority b)

-- ============================================================
-- Fingerprint Computation
-- ============================================================

-- | Compute fingerprint for an entire encoded module.
-- The hash is computed over all polynomial coefficients concatenated.
computeFingerprint :: EncodedModule -> FileFingerprint
computeFingerprint em =
  let -- Serialize all block coefficients to bytes
      coeffData = concatMap (serializeBlock . ebPoly) (emBlocks em)
      bs = BS.pack (map fromIntegral coeffData)
      -- Compute SHA-256
      digest = H.hash bs :: H.Digest H.SHA256
      hashBs = BS.pack (digestToWord8s digest)
      hashHex = concatMap byteToHex (BS.unpack hashBs)
      priority = hashToInteger hashBs
  in FileFingerprint
       { fpName     = emName em
       , fpHash     = hashBs
       , fpHashHex  = hashHex
       , fpPriority = priority
       }

-- | Compute fingerprint for a single block.
computeBlockFingerprint :: String -> EncodedBlock -> FileFingerprint
computeBlockFingerprint name block =
  let coeffData = serializeBlock (ebPoly block)
      bs = BS.pack (map fromIntegral coeffData)
      digest = H.hash bs :: H.Digest H.SHA256
      hashBs = BS.pack (digestToWord8s digest)
      hashHex = concatMap byteToHex (BS.unpack hashBs)
      priority = hashToInteger hashBs
  in FileFingerprint
       { fpName     = name ++ "#" ++ show (ebBlockId block)
       , fpHash     = hashBs
       , fpHashHex  = hashHex
       , fpPriority = priority
       }

-- ============================================================
-- Hash → Mathematical Priority
-- ============================================================

-- | Convert hash bytes to a large integer (big-endian).
-- This integer is used for mathematical ordering.
hashToInteger :: BS.ByteString -> Integer
hashToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

-- | Convert hash to a field element in GF(p).
hashToFieldElement :: Integer -> BS.ByteString -> FieldElement
hashToFieldElement p hashBs = mkFieldElement (hashToInteger hashBs `mod` p) p

-- ============================================================
-- Internal Helpers
-- ============================================================

-- | Extract polynomial coefficients as a list of integers.
serializeBlock :: BoundedPoly -> [Integer]
serializeBlock (BoundedPoly terms _ _) =
  concatMap (\(Term c e) -> [c, fromIntegral e]) terms

-- | Convert a Digest to [Word8].
digestToWord8s :: H.Digest H.SHA256 -> [Word8]
digestToWord8s digest = BS.unpack (digestToBS digest)

-- | Convert digest to ByteString using show+parse approach.
digestToBS :: H.Digest H.SHA256 -> BS.ByteString
digestToBS digest =
  let hexStr = show digest
  in BS.pack (hexStringToBytes hexStr)

-- | Parse hex string to bytes.
hexStringToBytes :: String -> [Word8]
hexStringToBytes [] = []
hexStringToBytes [_] = []
hexStringToBytes (a:b:rest) = hexPairToWord8 a b : hexStringToBytes rest

hexPairToWord8 :: Char -> Char -> Word8
hexPairToWord8 a b = fromIntegral (hexDigit a * 16 + hexDigit b)

hexDigit :: Char -> Int
hexDigit c
  | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise             = 0

byteToHex :: Word8 -> String
byteToHex b =
  let hi = b `div` 16
      lo = b `mod` 16
      hexChar n
        | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
        | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)
  in [hexChar hi, hexChar lo]
