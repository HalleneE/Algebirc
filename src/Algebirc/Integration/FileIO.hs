-- |
-- Module      : Algebirc.Integration.FileIO
-- Description : File-level universal binary obfuscator with Isogeny-CBC
-- License     : MIT

module Algebirc.Integration.FileIO
  ( -- * Pipeline
    obfuscateFile
  , processFile
  , deobfuscateFile
    -- * Core Processing
  , obfuscateBlob
  , deobfuscateData
    -- * Pipeline Construction
  , defaultPipeline
    -- * Metadata
  , ObfuscationData(..)
  ) where

import Algebirc.Core.Types
import Algebirc.Obfuscation.Encoder
import Algebirc.Obfuscation.Transform
import Algebirc.Obfuscation.NonlinearTransform (generatePipeline)
import Algebirc.Obfuscation.Pipeline
  ( ObfuscationPipeline(..), buildPipeline, runPipelinePoly, invertPipelinePoly, plAlgTransforms )
import Data.List (isPrefixOf)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import System.Directory (getPermissions, setPermissions, executable, setOwnerExecutable)
import System.FilePath (takeFileName)

-- ============================================================
-- Types
-- ============================================================

data ObfuscationData = ObfuscationData
  { odBlocks     :: ![(EncodedBlock, EncodedBlock)]  
  , odTransforms :: ![Transform]                      
  , odConfig     :: !ObfuscationConfig                
  , odFilename   :: !String                           
  , odIsExec     :: !Bool                     
  , odIV         :: !Integer
  , odOrigLen    :: !Int
  } deriving (Show)

-- ============================================================
-- File Pipeline
-- ============================================================

processFile :: ObfuscationConfig -> FilePath -> IO (Either String ObfuscationData)
processFile cfg inputPath = do
  perms <- getPermissions inputPath
  let isExec = executable perms
      fname  = takeFileName inputPath
  fileBytes <- BS.readFile inputPath
  obfuscateBlob cfg fname isExec fileBytes

obfuscateFile :: ObfuscationConfig -> FilePath -> FilePath -> IO (Either String ObfuscationData)
obfuscateFile cfg inputPath outputPath = do
  res <- processFile cfg inputPath
  case res of
    Left err -> return $ Left err
    Right od -> do
      let polyData = formatObfuscatedData (odBlocks od)
          outputContent = unlines
            [ "{- ALGEBIRC UNIVERSAL OBFUSCATED BLOB"
            , "   DO NOT EDIT MANUALLY"
            , ""
            , polyData
            , "-}"
            ]
      writeFile outputPath outputContent
      let metaPath = outputPath ++ ".meta"
          metaContent = serializeMetadata od
      writeFile metaPath metaContent
      return $ Right od

deobfuscateFile :: ObfuscationConfig -> FilePath -> FilePath -> IO (Either String ())
deobfuscateFile _cfg metaPath outputPath = do
  metaContent <- readFile metaPath
  case deserializeMetadata metaContent of
    Left err -> return $ Left $ "Metadata error: " ++ err
    Right (cfg, iv, origLen, isExec, blocks) ->
      case deobfuscateData cfg iv origLen blocks of
        Left err -> return $ Left $ "Deobfuscation error: " ++ show err
        Right recoveredBytes -> do
          BS.writeFile outputPath recoveredBytes
          basePerms <- getPermissions outputPath
          setPermissions outputPath (setOwnerExecutable isExec basePerms)
          return $ Right ()

-- ============================================================
-- In-Memory Universal Blob Processing
-- ============================================================

-- | Derive the chaining IV from an obfuscated polynomial.
--
-- Security rationale: instead of a linear sum of coefficients (trivially
-- predictable), we evaluate the polynomial at two key-derived points and
-- combine them nonlinearly. This makes IV(block_n) a quadratic function
-- of the ciphertext coefficients, not a linear one.
--
-- extractIV(f) = (f(seed mod p) * f(seed+1 mod p)) mod p
--
-- where seed is the config seed. This is:
--   1. Deterministic (same ciphertext + seed → same IV, required for deobfuscation)
--   2. Nonlinear in coefficients (IV ≠ sum of coefficients)
--   3. Pure (no IO required)
extractIV :: ObfuscationConfig -> BoundedPoly -> Integer
extractIV cfg (BoundedPoly terms maxDeg p) =
  let seed = cfgSeed cfg
      -- Evaluate poly at two seed-derived points
      evalAt x = foldl (\acc (Term c e) -> (acc + c * powMod x (fromIntegral e) p) `mod` p) 0 terms
      x0 = seed `mod` p
      x1 = (seed + 1) `mod` p
      v0 = evalAt x0
      v1 = evalAt x1
      -- Combine nonlinearly: product gives degree-2 mixing
      combined = (v0 * v1 + v0 + 1) `mod` p  -- +1 prevents IV=0 when poly is zero
  in combined
  where
    powMod _ 0 _ = 1
    powMod base ex md
      | even ex   = let h = powMod base (ex `div` 2) md in (h * h) `mod` md
      | otherwise = (base * powMod base (ex - 1) md) `mod` md

chainObfuscate :: ObfuscationConfig -> ObfuscationPipeline -> Integer -> [EncodedBlock] -> Either AlgebircError [(EncodedBlock, EncodedBlock)]
chainObfuscate cfg pl iv0 blocks =
  let go _ [] = Right []
      go iv (b:bs) = do
        let p = cfgFieldPrime cfg
            BoundedPoly terms md _ = ebPoly b
            maskedTerms = map (\(Term c e) -> Term ((c + iv) `mod` p) e) terms
            maskedPoly = mkBoundedPoly p md maskedTerms
        obfPoly <- runPipelinePoly cfg pl maskedPoly
        let nextIv = extractIV cfg obfPoly  -- nonlinear IV derivation
            obfBlock = b { ebPoly = obfPoly }
        rest <- go nextIv bs
        return ((b, obfBlock) : rest)
  in go iv0 blocks

chainDeobfuscate :: ObfuscationConfig -> ObfuscationPipeline -> Integer -> [EncodedBlock] -> Either AlgebircError [EncodedBlock]
chainDeobfuscate cfg pl iv0 obfBlocks =
  let go _ [] = Right []
      go iv (ob:obs) = do
        maskedPoly <- invertPipelinePoly cfg pl (ebPoly ob)
        let p = cfgFieldPrime cfg
            BoundedPoly terms md _ = maskedPoly
            unmaskedTerms = map (\(Term c e) -> Term ((c - iv) `mod` p) e) terms
            origPoly = mkBoundedPoly p md unmaskedTerms
            origBlock = ob { ebPoly = origPoly }
            nextIv = extractIV cfg (ebPoly ob)  -- derive same IV as during obfuscation
        rest <- go nextIv obs
        return (origBlock : rest)
  in go iv0 obfBlocks

obfuscateBlob :: ObfuscationConfig -> String -> Bool -> BS.ByteString -> IO (Either String ObfuscationData)
obfuscateBlob cfg fname isExec fileBytes = do
  case buildPipeline cfg of
    Left err -> return (Left $ show err)
    Right pl -> do
      blocks <- encodeStream cfg fileBytes
      ivBytes <- getRandomBytes 16 :: IO BS.ByteString
      let iv = foldl (\acc b -> acc * 256 + fromIntegral b) 0 (BS.unpack ivBytes)
      let transforms = plAlgTransforms pl
      case chainObfuscate cfg pl iv blocks of
        Left err -> return (Left $ show err)
        Right blockPairs ->
          return $ Right ObfuscationData
            { odBlocks     = blockPairs
            , odTransforms = transforms
            , odConfig     = cfg
            , odFilename   = fname
            , odIsExec     = isExec
            , odIV         = iv
            , odOrigLen    = BS.length fileBytes
            }

deobfuscateData :: ObfuscationConfig -> Integer -> Int -> [EncodedBlock] -> Either AlgebircError BS.ByteString
deobfuscateData cfg iv origLen blocks = do
  pl <- buildPipeline cfg
  origBlocks <- chainDeobfuscate cfg pl iv blocks
  return $ decodeStream cfg origBlocks origLen

-- ============================================================
-- Default Pipeline
-- ============================================================

defaultPipeline :: ObfuscationConfig -> [Transform]
defaultPipeline cfg =
  let seed = cfgSeed cfg
      sk = SecretKey
           { skSeed     = seed
           , skPowerExp = 3
           }
  in case generatePipeline cfg sk of
       Right transforms -> transforms
       Left _ -> [] 

-- ============================================================
-- Serialization
-- ============================================================

serializeMetadata :: ObfuscationData -> String
serializeMetadata od =
  unlines
    [ "-- Algebirc Universal Metadata"
    , "-- Origin File: " ++ odFilename od
    , "-- Executable: " ++ show (odIsExec od)
    , "-- Field Prime: " ++ show (cfgFieldPrime (odConfig od))
    , "-- Max Degree: " ++ show (cfgMaxDegree (odConfig od))
    , "-- Seed: " ++ show (cfgSeed (odConfig od))
    , "-- Genus: " ++ show (cfgGenus (odConfig od))
    , "-- CBC IV: " ++ show (odIV od)
    , "-- Source Length: " ++ show (odOrigLen od)
    , "-- Transform Count: " ++ show (length (odTransforms od))
    , "-- Block Count: " ++ show (length (odBlocks od))
    , "---BEGIN OBFUSCATED BLOCKS---"
    , formatObfuscatedData (odBlocks od)
    , "---END OBFUSCATED BLOCKS---"
    ]

formatObfuscatedData :: [(EncodedBlock, EncodedBlock)] -> String
formatObfuscatedData blocks = unlines $ map fmtBlock blocks
  where
    fmtBlock (_, obf) =
      let poly = ebPoly obf
          coeffs = polyCoefficients poly
      in "BLOCK " ++ show (ebBlockId obf) ++ ": " ++ show coeffs

deserializeMetadata :: String -> Either String (ObfuscationConfig, Integer, Int, Bool, [EncodedBlock])
deserializeMetadata content =
  let ls = lines content
  in case break (== "---BEGIN OBFUSCATED BLOCKS---") ls of
       (headerLines, _marker:rest) ->
         case break (== "---END OBFUSCATED BLOCKS---") rest of
           (blockLines, _endMark:_) ->
             let seed = extractSeed headerLines
                 prime = extractPrime headerLines
                 maxDeg = extractMaxDeg headerLines
                 genus = extractGenus headerLines
                 iv = extractNum "-- CBC IV: " 0 headerLines
                 origLen = fromIntegral $ extractNum "-- Source Length: " 0 headerLines
                 isExec = extractBool "-- Executable: " False headerLines
                 cfg = defaultConfig
                         { cfgFieldPrime = prime
                         , cfgMaxDegree  = maxDeg
                         , cfgSeed       = seed
                         , cfgGenus      = genus
                         }
                 blocks = parseBlocks prime maxDeg blockLines
             in Right (cfg, iv, origLen, isExec, blocks)
           _ -> Left "Missing ---END OBFUSCATED BLOCKS--- marker"
       _ -> Left "Missing ---BEGIN OBFUSCATED BLOCKS--- marker"

parseBlocks :: Integer -> Int -> [String] -> [EncodedBlock]
parseBlocks prime maxDeg ls =
  [ let (idStr, rest) = break (== ':') (drop 6 l)
        bid = read idStr :: Int
        coeffTuples = read (drop 2 rest) :: [(Int, Integer)]
        terms = map (\(d, c) -> Term c d) coeffTuples
        poly = mkBoundedPoly prime maxDeg terms
    in EncodedBlock poly (maxDeg+1) bid
  | l <- ls, "BLOCK " `isPrefixOf` l ]

extractSeed :: [String] -> Integer
extractSeed = extractNum "-- Seed: " 42

extractPrime :: [String] -> Integer
extractPrime = extractNum "-- Field Prime: " 257

extractMaxDeg :: [String] -> Int
extractMaxDeg ls = fromIntegral $ extractNum "-- Max Degree: " 64 ls

extractGenus :: [String] -> Int
extractGenus ls = fromIntegral $ extractNum "-- Genus: " 1 ls

extractNum :: String -> Integer -> [String] -> Integer
extractNum prefix def ls =
  case filter (\l -> take (length prefix) l == prefix) ls of
    (l:_) -> case reads (drop (length prefix) l) of
               [(n, _)] -> n
               _ -> def
    [] -> def

extractBool :: String -> Bool -> [String] -> Bool
extractBool prefix def ls =
  case filter (\l -> take (length prefix) l == prefix) ls of
    (l:_) -> l == prefix ++ "True"
    [] -> def
