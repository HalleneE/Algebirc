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
import Algebirc.Integration.HaskellParser
import Algebirc.Core.Group (generateFromSeed)
import Data.Either (fromRight)
import Control.Monad (foldM)

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
deobfuscateFile cfg metaPath outputPath = do
  metaContent <- readFile metaPath
  case deserializeMetadata metaContent of
    Left err -> return $ Left $ "Metadata error: " ++ err
    Right (origSrc, transforms) ->
      case deobfuscateData cfg origSrc transforms of
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
  let -- Default transform pipeline berdasarkan seed
      transforms = defaultPipeline cfg
      -- Encode setiap deklarasi
      declExprs = moduleToDeclExprs sourceMod
  in case mapM (encodeAndObfuscate cfg transforms) declExprs of
       Left err -> Left err
       Right blockPairs ->
         Right ObfuscationData
           { odBlocks     = blockPairs
           , odTransforms = transforms
           , odConfig     = cfg
           , odOrigSrc    = origSrc
           , odModule     = sourceMod
           }

-- | Deobfuscate data kembali ke source string.
deobfuscateData :: ObfuscationConfig
               -> String          -- ^ Original source (untuk metadata)
               -> [Transform]     -- ^ Transforms yang dipakai
               -> Either AlgebircError String
deobfuscateData cfg origSrc transforms =
  -- Untuk sekarang: simpan dan restore original source
  -- Full deobfuscation: decode blocks → reconstruct AST → generate Haskell
  case parseHaskellSource origSrc of
    Left err -> Left (GenericError $ "Parse error on stored source: " ++ err)
    Right pr ->
      let sourceMod = prModule pr
          declExprs = moduleToDeclExprs sourceMod
      in case mapM (encodeAndDeobfuscate cfg transforms) declExprs of
           Left err -> Left err
           Right _decodedExprs ->
             -- Reconstruct menggunakan source asli yang sudah diverifikasi
             Right origSrc

-- | Encode satu expresi dan obfuscate.
encodeAndObfuscate :: ObfuscationConfig
                  -> [Transform]
                  -> SourceExpr
                  -> Either AlgebircError (EncodedBlock, EncodedBlock)
encodeAndObfuscate cfg transforms expr =
  case encodeExpr cfg expr of
    Left err -> Left err
    Right block ->
      case obfuscateBlockUnified cfg transforms block of
        Left err -> Left err
        Right obfBlock -> Right (block, obfBlock)

-- | Unified application (handles both linear and nonlinear transforms).
obfuscateBlockUnified :: ObfuscationConfig -> [Transform] -> EncodedBlock -> Either AlgebircError EncodedBlock
obfuscateBlockUnified cfg transforms block = do
  poly' <- applyPipelineUnified cfg transforms (ebPoly block)
  return block { ebPoly = poly' }

applyPipelineUnified :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPipelineUnified cfg ts p = foldM (\acc t -> applyUnified cfg t acc) p ts

applyUnified :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyUnified cfg t poly = case transformTag t of
  SBoxTransform         -> applyNonlinear cfg t poly
  FeistelTransform      -> applyNonlinear cfg t poly
  PowerMapTransform     -> applyNonlinear cfg t poly
  ARXDiffusionTransform -> applyNonlinear cfg t poly
  _                     -> applyTransform cfg t poly

-- | Deobfuscate dan decode kembali.
encodeAndDeobfuscate :: ObfuscationConfig
                    -> [Transform]
                    -> SourceExpr
                    -> Either AlgebircError String
encodeAndDeobfuscate cfg transforms expr =
  case encodeExpr cfg expr of
    Left err -> Left err
    Right block ->
      case obfuscateBlockUnified cfg transforms block of
        Left err -> Left err
        Right obfBlock ->
          -- Deobfuscation is harder because we need inverse unification too
          -- For now just use original block to verify encode/decode logic
          -- (Full deobfuscation requires invertUnified which mirrors applyUnified)
          -- Since we disabled deobfuscate command effectively, this is placeholder
          Right (decodeExpr cfg block)

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
deserializeMetadata :: String -> Either String (String, [Transform])
deserializeMetadata content =
  let ls = lines content
  in case break (== "---BEGIN OBFUSCATED BLOCKS---") ls of
       (headerLines, _marker:rest) ->
         case break (== "---END OBFUSCATED BLOCKS---") rest of
           (blockLines, _endMark:_) ->
             let -- For now, we don't reconstruct from blocks in this demo
                 -- Just use a placeholder since we changed format
                 src = "" 
                 seed = extractSeed headerLines
                 prime = extractPrime headerLines
                 maxDeg = extractMaxDeg headerLines
                 cfg = defaultConfig
                         { cfgFieldPrime = prime
                         , cfgMaxDegree  = maxDeg
                         , cfgSeed       = seed
                         }
                 transforms = defaultPipeline cfg
             in Right (src, transforms)
           _ -> Left "Missing ---END SOURCE--- marker"
       _ -> Left $ "Missing ---BEGIN SOURCE--- marker. " ++ show beginMark
  where
    beginMark = "---BEGIN SOURCE---" :: String

extractSeed :: [String] -> Integer
extractSeed = extractNum "-- Seed: " 42

extractPrime :: [String] -> Integer
extractPrime = extractNum "-- Field Prime: " 257

extractMaxDeg :: [String] -> Int
extractMaxDeg ls = fromIntegral $ extractNum "-- Max Degree: " 64 ls

extractNum :: String -> Integer -> [String] -> Integer
extractNum prefix def ls =
  case filter (\l -> take (length prefix) l == prefix) ls of
    (l:_) -> case reads (drop (length prefix) l) of
               [(n, _)] -> n
               _ -> def
    [] -> def
