-- |
-- Module      : Algebirc.Integration.FileIO
-- Description : File-level obfuscation/deobfuscation pipeline
-- License     : MIT
--
-- Pipeline lengkap: baca .hs → parse → encode → transform → simpan
-- Juga: baca obfuscated → decode → reconstruct → tulis .hs
--
-- Format output: dua file per obfuscation:
-- * @.algebirc@ — obfuscated polynomial data (binary-safe JSON)
-- * @.algebirc.meta@ — metadata (transform keys, original size)

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

-- ============================================================
-- Types
-- ============================================================

-- | Data obfuscation: berisi polynomial + metadata untuk deobfuscate.
data ObfuscationData = ObfuscationData
  { odBlocks     :: ![(EncodedBlock, EncodedBlock)]  -- ^ (original, obfuscated) pairs
  , odTransforms :: ![Transform]                      -- ^ Transforms yang dipakai
  , odConfig     :: !ObfuscationConfig                -- ^ Config
  , odOrigSrc    :: !String                           -- ^ Source asli (untuk verifikasi)
  , odModule     :: !SourceModule                     -- ^ Parsed module
  } deriving (Show)

-- ============================================================
-- File Pipeline
-- ============================================================

-- | Parse and obfuscate file without writing output.
processFile :: ObfuscationConfig -> FilePath -> IO (Either String ObfuscationData)
processFile cfg inputPath = do
  parseResult <- parseHaskellFile inputPath
  case parseResult of
    Left err -> return $ Left $ "Parse error: " ++ err
    Right pr -> do
      return $ case obfuscateSource cfg (prOriginal pr) (prModule pr) of
        Left err -> Left $ "Obfuscation error: " ++ show err
        Right od -> Right od

-- | Obfuscate file .hs.
-- Baca source → parse → encode → transform → simpan hasil.
obfuscateFile :: ObfuscationConfig
             -> FilePath         -- ^ Input .hs file
             -> FilePath         -- ^ Output file (obfuscated .hs)
             -> IO (Either String ObfuscationData)
obfuscateFile cfg inputPath outputPath = do
  res <- processFile cfg inputPath
  case res of
    Left err -> return $ Left err
    Right od -> do
      -- Tulis obfuscated source
      -- Tulis obfuscated source (sebagai komentar polynomial data + reconstructed source)
      let obfHaskell = sourceToHaskell (odModule od)
          polyData = formatObfuscatedData (odBlocks od)
          outputContent = unlines
            [ "{- ALGEBIRC OBFUSCATED DATA"
            , "   DO NOT EDIT MANUALLY"
            , ""
            , polyData
            , "-}"
            , ""
            , "-- Reconstructed Source (for verification):"
            , obfHaskell
            ]
      writeFile outputPath outputContent
      -- Juga tulis metadata untuk deobfuscation
      let metaPath = outputPath ++ ".meta"
          metaContent = serializeMetadata od
      writeFile metaPath metaContent
      return $ Right od

-- | Deobfuscate: baca obfuscated data → reconstruct source.
deobfuscateFile :: ObfuscationConfig
               -> FilePath        -- ^ Input meta file (.algebirc.meta)
               -> FilePath        -- ^ Output .hs file (recovered)
               -> IO (Either String String)
deobfuscateFile _cfg metaPath outputPath = do
  metaContent <- readFile metaPath
  case deserializeMetadata metaContent of
    Left err -> return $ Left $ "Metadata error: " ++ err
    Right (cfg, blocks) ->
      case deobfuscateData cfg blocks of
        Left err -> return $ Left $ "Deobfuscation error: " ++ show err
        Right recovered -> do
          writeFile outputPath recovered
          return $ Right recovered

-- ============================================================
-- In-Memory Pipeline
-- ============================================================

-- | Obfuscate source code in memory.
obfuscateSource :: ObfuscationConfig
               -> String           -- ^ Original source
               -> SourceModule     -- ^ Parsed module
               -> Either AlgebircError ObfuscationData
obfuscateSource cfg origSrc sourceMod =
  case buildPipeline cfg of
    Left err -> Left err
    Right pl ->
      let transforms  = plAlgTransforms pl
          declExprs   = moduleToDeclExprs sourceMod
      in case mapM (encodeAndObfuscate cfg pl) declExprs of
           Left err -> Left err
           Right blockPairs ->
             Right ObfuscationData
               { odBlocks     = blockPairs
               , odTransforms = transforms
               , odConfig     = cfg
               , odOrigSrc    = origSrc
               , odModule     = sourceMod
               }

-- | Deobfuscate data recovered from encoded blocks strings.
deobfuscateData :: ObfuscationConfig -> [EncodedBlock] -> Either AlgebircError String
deobfuscateData cfg blocks = do
  pl <- buildPipeline cfg
  let decodeOne block = do
        origPoly <- invertPipelinePoly cfg pl (ebPoly block)
        return $ decodeExpr cfg (block { ebPoly = origPoly })
  decodedStrings <- mapM decodeOne blocks
  return $ unlines decodedStrings

-- | Encode satu expresi dan obfuscate.
encodeAndObfuscate :: ObfuscationConfig
                  -> ObfuscationPipeline
                  -> SourceExpr
                  -> Either AlgebircError (EncodedBlock, EncodedBlock)
encodeAndObfuscate cfg pl expr =
  case encodeExpr cfg expr of
    Left err -> Left err
    Right block ->
      case runPipelinePoly cfg pl (ebPoly block) of
        Left err -> Left err
        Right obfPoly -> Right (block, block { ebPoly = obfPoly })

-- removed redundant unified handlers from FileIO, use Pipeline layers.

-- ============================================================
-- Default Pipeline
-- ============================================================

-- | Generate pipeline standar dari config seed.
defaultPipeline :: ObfuscationConfig -> [Transform]
defaultPipeline cfg =
  let seed = cfgSeed cfg
      -- Create secret key from seed
      sk = SecretKey
           { skSeed     = seed
           , skRounds   = 4  -- 4 rounds Feistel
           , skSBoxSeed = seed + 1
           , skPowerExp = 3
           }
  in case generatePipeline cfg sk of
       Right transforms -> transforms
       Left _ -> [] -- Should not happen with valid config

-- ============================================================
-- Helpers
-- ============================================================

-- | Extract semua expressions dari module declarations.
moduleToDeclExprs :: SourceModule -> [SourceExpr]
moduleToDeclExprs (SourceModule _ decls) = map declToExpr decls
  where
    declToExpr (FuncDecl _ _ body)  = body
    declToExpr (ValDecl _ body)     = body

-- | Serialize metadata untuk menyimpan transform info.
serializeMetadata :: ObfuscationData -> String
serializeMetadata od =
  unlines
    [ "-- Algebirc Obfuscation Metadata"
    , "-- DO NOT EDIT"
    , "-- Field Prime: " ++ show (cfgFieldPrime (odConfig od))
    , "-- Max Degree: " ++ show (cfgMaxDegree (odConfig od))
    , "-- Seed: " ++ show (cfgSeed (odConfig od))
    , "-- Genus: " ++ show (cfgGenus (odConfig od))
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


-- | Deserialize metadata.
deserializeMetadata :: String -> Either String (ObfuscationConfig, [EncodedBlock])
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
                 cfg = defaultConfig
                         { cfgFieldPrime = prime
                         , cfgMaxDegree  = maxDeg
                         , cfgSeed       = seed
                         , cfgGenus      = genus
                         }
                 blocks = parseBlocks prime maxDeg blockLines
             in Right (cfg, blocks)
           _ -> Left "Missing ---END OBFUSCATED BLOCKS--- marker"
       _ -> Left "Missing ---BEGIN OBFUSCATED BLOCKS--- marker"

parseBlocks :: Integer -> Int -> [String] -> [EncodedBlock]
parseBlocks prime maxDeg ls =
  [ let (idStr, rest) = break (== ':') (drop 6 l)
        bid = read idStr :: Int
        coeffs = read (drop 2 rest) :: [Integer]
        terms = zipWith (\i c -> Term c i) [0..] coeffs
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
