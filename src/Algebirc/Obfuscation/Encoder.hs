-- |
-- Module      : Algebirc.Obfuscation.Encoder
-- Description : Encode source AST into algebraic objects
-- License     : MIT
--
-- Converts source expressions into field elements and polynomial
-- representations suitable for algebraic transformation.

module Algebirc.Obfuscation.Encoder
  ( -- * Encoding
    encodeExpr
  , encodeModule
  , decodeExpr
  , decodeModule
  , encodeStream
  , decodeStream
    -- * Byte-Level
  , bytesToFieldElements
  , fieldElementsToBytes
    -- * Types
  , EncodedBlock(..)
  , EncodedModule(..)
  ) where

import Algebirc.Core.Types
import Algebirc.Obfuscation.AST
import qualified Data.ByteString as BS
import Data.Char (ord, chr)
import Crypto.Random (getRandomBytes)

-- ============================================================
-- Encoded Types
-- ============================================================

-- | A block of encoded data: the serialized AST encoded as
-- polynomial coefficients over GF(p).
data EncodedBlock = EncodedBlock
  { ebPoly      :: !BoundedPoly      -- ^ Data encoded as polynomial
  , ebOrigSize  :: !Int              -- ^ Original byte count
  , ebBlockId   :: !Int              -- ^ Block index
  } deriving (Show, Eq)

-- | An encoded module: all blocks plus metadata.
data EncodedModule = EncodedModule
  { emName      :: !String
  , emBlocks    :: ![EncodedBlock]
  , emConfig    :: !ObfuscationConfig  -- ^ FROZEN config
  , emBlockSize :: !Int               -- ^ Bytes per block
  } deriving (Show, Eq)

-- ============================================================
-- Byte ↔ Field Element Conversion
-- ============================================================

-- | Convert a ByteString to a list of field elements.
-- Each byte becomes one field element in GF(p).
bytesToFieldElements :: Integer -> BS.ByteString -> [FieldElement]
bytesToFieldElements p bs =
  map (\b -> mkFieldElement (fromIntegral b) p) (BS.unpack bs)

-- | Convert field elements back to bytes.
-- Clamps values to [0, 255] range.
fieldElementsToBytes :: [FieldElement] -> BS.ByteString
fieldElementsToBytes fes =
  BS.pack $ map (\fe -> fromIntegral (feValue fe `mod` 256)) fes

-- ============================================================
-- AST → Polynomial Encoding
-- ============================================================

-- | Encode a source expression into an algebraic representation.
--
-- Strategy: serialize the AST to bytes, then encode bytes as
-- polynomial coefficients over GF(p).
--
-- Each byte b_i at position i becomes the coefficient of x^i.
-- So the polynomial is: f(x) = b_0 + b_1*x + b_2*x^2 + ...
encodeExpr :: ObfuscationConfig -> SourceExpr -> Either AlgebircError EncodedBlock
encodeExpr cfg expr =
  let serialized = serializeExpr expr
      bytes      = map (fromIntegral . ord) serialized
      p          = cfgFieldPrime cfg
      maxDeg     = cfgMaxDegree cfg
      -- Create terms: byte_i * x^i
      terms      = zipWith (\b i -> Term b i) bytes [0..]
  in if length bytes - 1 > maxDeg
     then Left (DegreeOverflow (length bytes - 1) maxDeg)
     else Right EncodedBlock
            { ebPoly     = mkBoundedPoly p maxDeg terms
            , ebOrigSize = length bytes
            , ebBlockId  = 0
            }

-- | Encode an entire module, splitting into blocks if needed.
encodeModule :: ObfuscationConfig -> SourceModule -> Either AlgebircError EncodedModule
encodeModule cfg srcMod =
  let serialized = serializeModule srcMod
      p          = cfgFieldPrime cfg
      maxDeg     = cfgMaxDegree cfg
      blockSize  = maxDeg + 1  -- max bytes per block
      -- Split into chunks
      chunks     = chunkString blockSize serialized
      -- Encode each chunk as a polynomial
      encodeChunk idx chunk =
        let bytes = map (fromIntegral . ord) chunk
            terms = zipWith (\b i -> Term b i) bytes [0..]
        in EncodedBlock
             { ebPoly     = mkBoundedPoly p maxDeg terms
             , ebOrigSize = length bytes
             , ebBlockId  = idx
             }
      blocks = zipWith encodeChunk [0..] chunks
  in Right EncodedModule
       { emName      = smName srcMod
       , emBlocks    = blocks
       , emConfig    = cfg
       , emBlockSize = blockSize
       }

-- ============================================================
-- Polynomial → AST Decoding
-- ============================================================

-- | Decode a polynomial back to a source expression string.
-- Evaluates the polynomial at x=0,1,2,... to recover coefficients
-- (which are the original bytes).
decodeExpr :: ObfuscationConfig -> EncodedBlock -> String
decodeExpr _cfg block =
  let poly = ebPoly block
      n    = ebOrigSize block
      -- Extract coefficients by position (getCoeffAt is in Types.hs)
      bytes = [ getCoeffAt i poly | i <- [0 .. n - 1] ]
  in map (chr . fromIntegral . (`mod` 256)) bytes

-- | Decode an entire module.
decodeModule :: EncodedModule -> String
decodeModule em =
  let cfg = emConfig em
  in concatMap (decodeExpr cfg) (emBlocks em)

-- ============================================================
-- Stream Encoding with True Random Padding
-- ============================================================

-- | Encode a string stream into exactly maxDeg-sized blocks with True OS Random padding.
encodeStream :: ObfuscationConfig -> String -> IO [EncodedBlock]
encodeStream cfg srcStr = do
  let bytes = map (fromIntegral . ord) srcStr
      p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      blockSize = maxDeg + 1

  let chunkList [] = return []
      chunkList bs = do
        let (chunk, rest) = splitAt blockSize bs
        if length chunk == blockSize
          then do
            rest' <- chunkList rest
            return (chunk : rest')
          else do
            let padLen = blockSize - length chunk
            padBytes <- getRandomBytes padLen :: IO BS.ByteString
            let padInts = map fromIntegral (BS.unpack padBytes)
            return [chunk ++ padInts]

  chunks <- chunkList bytes
  let makeBlock idx chunk =
        let terms = zipWith (\b i -> Term b i) chunk [0 .. maxDeg]
            poly = mkBoundedPoly p maxDeg terms
        in EncodedBlock poly blockSize idx
  return $ zipWith makeBlock [0..] chunks

-- | Decode blocks and truncate to original length (binning the toxic padding).
decodeStream :: ObfuscationConfig -> [EncodedBlock] -> Int -> String
decodeStream cfg blocks origLen =
  let p = cfgFieldPrime cfg
      decodeOne (EncodedBlock poly _ _) =
        [ getCoeffAt i poly | i <- [0 .. cfgMaxDegree cfg] ]
      allBytes = concatMap decodeOne blocks
      validBytes = take origLen allBytes
  in map (chr . fromIntegral . (`mod` 256)) validBytes

-- ============================================================
-- Helpers
-- ============================================================

chunkString :: Int -> String -> [String]
chunkString _ [] = []
chunkString n s  = take n s : chunkString n (drop n s)

-- Re-export for Encoder usage
serializeExpr :: SourceExpr -> String
serializeExpr (SLit n)        = show n
serializeExpr (SVar v)        = v
serializeExpr (SApp f a)      = "(app " ++ serializeExpr f ++ " " ++ serializeExpr a ++ ")"
serializeExpr (SLam p b)      = "(lam " ++ p ++ " " ++ serializeExpr b ++ ")"
serializeExpr (SLet v e1 e2)  = "(let " ++ v ++ " " ++ serializeExpr e1 ++ " " ++ serializeExpr e2 ++ ")"
serializeExpr (SIf c t f)     = "(if " ++ serializeExpr c ++ " " ++ serializeExpr t ++ " " ++ serializeExpr f ++ ")"
serializeExpr (SBinOp op l r) = "(" ++ op ++ " " ++ serializeExpr l ++ " " ++ serializeExpr r ++ ")"
serializeExpr (SList xs)      = "(list " ++ unwords (map serializeExpr xs) ++ ")"
serializeExpr (STuple xs)     = "(tuple " ++ unwords (map serializeExpr xs) ++ ")"
serializeExpr (SBlock xs)     = "(block " ++ unwords (map serializeExpr xs) ++ ")"
