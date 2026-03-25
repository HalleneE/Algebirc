-- |
-- Module      : Algebirc.Integration.FileIO
-- Description : File-level obfuscation/deobfuscation pipeline with Isogeny-CBC
-- License     : MIT

module Algebirc.Integration.FileIO
  ( -- * Pipeline
    obfuscateFile
  , processFile
  , deobfuscateFile
    -- * In-Memory Pipeline
  , obfuscateSource
  , deobfuscateData
    -- * Pipeline Construction
  , defaultPipeline
    -- * Metadata
  , ObfuscationData(..)
  ) where

import Algebirc.Core.Types
import Algebirc.Obfuscation.AST
import Algebirc.Obfuscation.Encoder
import Algebirc.Obfuscation.Transform
import Algebirc.Obfuscation.NonlinearTransform (generatePipeline, applyNonlinear)
import Algebirc.Obfuscation.Pipeline
  ( ObfuscationPipeline(..), buildPipeline, runPipelinePoly, invertPipelinePoly, plAlgTransforms )
import Algebirc.Integration.HaskellParser
import Algebirc.Core.Group (generateFromSeed)
import Data.Either (fromRight)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS

-- ============================================================
-- Types
-- ============================================================

data ObfuscationData = ObfuscationData
  { odBlocks     :: ![(EncodedBlock, EncodedBlock)]  
  , odTransforms :: ![Transform]                      
  , odConfig     :: !ObfuscationConfig                
  , odOrigSrc    :: !String                           
  , odModule     :: !SourceModule                     
  , odIV         :: !Integer
  , odOrigLen    :: !Int
  } deriving (Show)

-- ============================================================
-- File Pipeline
-- ============================================================

processFile :: ObfuscationConfig -> FilePath -> IO (Either String ObfuscationData)
processFile cfg inputPath = do
  parseResult <- parseHaskellFile inputPath
  case parseResult of
    Left err -> return $ Left $ "Parse error: " ++ err
    Right pr -> do
      res <- obfuscateSource cfg (prOriginal pr) (prModule pr)
      case res of
        Left err -> return $ Left $ "Obfuscation error: " ++ show err
        Right od -> return $ Right od

obfuscateFile :: ObfuscationConfig -> FilePath -> FilePath -> IO (Either String ObfuscationData)
obfuscateFile cfg inputPath outputPath = do
  res <- processFile cfg inputPath
  case res of
    Left err -> return $ Left err
    Right od -> do
      let polyData = formatObfuscatedData (odBlocks od)
          outputContent = unlines
            [ "{- ALGEBIRC OBFUSCATED DATA"
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

deobfuscateFile :: ObfuscationConfig -> FilePath -> FilePath -> IO (Either String String)
deobfuscateFile _cfg metaPath outputPath = do
  metaContent <- readFile metaPath
  case deserializeMetadata metaContent of
    Left err -> return $ Left $ "Metadata error: " ++ err
    Right (cfg, iv, origLen, blocks) ->
      case deobfuscateData cfg iv origLen blocks of
        Left err -> return $ Left $ "Deobfuscation error: " ++ show err
        Right recovered -> do
          writeFile outputPath recovered
          return $ Right recovered

-- ============================================================
-- In-Memory Pipeline (Flattened + OS Entropy Padding)
-- ============================================================

extractIV :: BoundedPoly -> Integer
extractIV (BoundedPoly terms _ p) = sum (map termCoeff terms) `mod` p

chainObfuscate :: ObfuscationConfig -> ObfuscationPipeline -> Integer -> [EncodedBlock] -> Either AlgebircError [(EncodedBlock, EncodedBlock)]
chainObfuscate cfg pl iv0 blocks =
  let go _ [] = Right []
      go iv (b:bs) = do
        let p = cfgFieldPrime cfg
            BoundedPoly terms md _ = ebPoly b
            maskedTerms = map (\(Term c e) -> Term ((c + iv) `mod` p) e) terms
            maskedPoly = mkBoundedPoly p md maskedTerms
        obfPoly <- runPipelinePoly cfg pl maskedPoly
        let nextIv = extractIV obfPoly
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
            nextIv = extractIV (ebPoly ob)
        rest <- go nextIv obs
        return (origBlock : rest)
  in go iv0 obfBlocks

obfuscateSource :: ObfuscationConfig -> String -> SourceModule -> IO (Either AlgebircError ObfuscationData)
obfuscateSource cfg origSrc sourceMod = do
  case buildPipeline cfg of
    Left err -> return (Left err)
    Right pl -> do
      blocks <- encodeStream cfg origSrc
      ivBytes <- getRandomBytes 16 :: IO BS.ByteString
      let iv = foldl (\acc b -> acc * 256 + fromIntegral b) 0 (BS.unpack ivBytes)
      let transforms = plAlgTransforms pl
      case chainObfuscate cfg pl iv blocks of
        Left err -> return (Left err)
        Right blockPairs ->
          return $ Right ObfuscationData
            { odBlocks     = blockPairs
            , odTransforms = transforms
            , odConfig     = cfg
            , odOrigSrc    = origSrc
            , odModule     = sourceMod
            , odIV         = iv
            , odOrigLen    = length origSrc
            }

deobfuscateData :: ObfuscationConfig -> Integer -> Int -> [EncodedBlock] -> Either AlgebircError String
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
           , skRounds   = 4 
           , skSBoxSeed = seed + 1
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
    [ "-- Algebirc Obfuscation Metadata"
    , "-- DO NOT EDIT"
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

deserializeMetadata :: String -> Either String (ObfuscationConfig, Integer, Int, [EncodedBlock])
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
                 cfg = defaultConfig
                         { cfgFieldPrime = prime
                         , cfgMaxDegree  = maxDeg
                         , cfgSeed       = seed
                         , cfgGenus      = genus
                         }
                 blocks = parseBlocks prime maxDeg blockLines
             in Right (cfg, iv, origLen, blocks)
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
