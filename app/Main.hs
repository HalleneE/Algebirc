module Main where

import System.Environment (getArgs)
import Algebirc.Core.Types
import Algebirc.Integration.FileIO
import Algebirc.Analysis.Invertibility
import Algebirc.Analysis.Leakage

import System.IO (hFlush, stdout)
import Crypto.Hash (SHA256(..), hashWith)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

-- | Hash iteration for deriving a secure 256-bit seed from password
deriveSeed :: String -> Integer
deriveSeed pass =
    let salt = B8.pack "algebirc_salt_v1"
        loop :: Int -> BS.ByteString -> BS.ByteString
        loop 0 h = h
        loop n h = loop (n - 1) (BA.convert (hashWith SHA256 h) :: BS.ByteString)
        finalHash = loop 10000 (B8.pack pass `BS.append` salt)
    in foldl (\acc b -> acc * 256 + fromIntegral b) 0 (BS.unpack finalHash)

promptPassword :: Maybe String -> IO String
promptPassword (Just p) = return p
promptPassword Nothing = do
    putStr "Enter Obfuscation Passphrase: "
    hFlush stdout
    getLine

parsePassword :: [String] -> (Maybe String, [String])
parsePassword [] = (Nothing, [])
parsePassword ("--password" : p : rest) = (Just p, rest)
parsePassword ("-p" : p : rest) = (Just p, rest)
parsePassword (x:xs) =
  let (pass, rest) = parsePassword xs
  in (pass, x : rest)

-- | Parse optional --genus N from arg list, returning (genus, remaining args).
parseGenus :: [String] -> (Int, [String])
parseGenus ("--genus" : n : rest) = (read n, rest)
parseGenus args                   = (1, args)

main :: IO ()
main = do
  args0 <- getArgs
  let (mPass, args) = parsePassword args0
  case args of
    ("obfuscate" : inputFile : rest) -> do
      pass <- promptPassword mPass
      let (genusN, rest1) = parseGenus rest
          outputFile = case rest1 of
            ["-o", out] -> out
            _           -> inputFile ++ ".obf"
          seed = deriveSeed pass
          cfg = defaultConfig { cfgGenus = genusN, cfgSeed = seed }
      runObfuscate cfg inputFile outputFile

    ("deobfuscate" : metaFile : rest) -> do
      pass <- promptPassword mPass
      let (genusN, rest1) = parseGenus rest
          outputFile = case rest1 of
            ["-o", out] -> out
            _           -> "recovered_file"
          seed = deriveSeed pass
          cfg = defaultConfig { cfgGenus = genusN, cfgSeed = seed }
      runDeobfuscate cfg metaFile outputFile

    ("analyze" : inputFile : _) ->
      runAnalyze inputFile

    ("help" : _) ->
      printUsage

    [] -> do
      printUsage

    _ -> do
      putStrLn "Error: Unrecognized command directive."
      printUsage

-- ============================================================
-- CLI Commands
-- ============================================================

printUsage :: IO ()
printUsage = do
  putStrLn "╔══════════════════════════════════════════════════════════════════╗"
  putStrLn "║  Algebirc Universal: Algebraic Binary Obfuscation Engine (G-2)   ║"
  putStrLn "╚══════════════════════════════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "SYNOPSIS:"
  putStrLn "  algebirc obfuscate <input.bin> [--genus 2] [-o out]     -- Universal Binary Obfuscation"
  putStrLn "  algebirc deobfuscate <file.meta> [--genus 2] [-o rec]   -- Recover from obfuscation metadata"
  putStrLn "  algebirc analyze <input.bin>                            -- Algebraic & structural metric analysis"
  putStrLn "  algebirc help                                           -- This help"
  putStrLn ""
  putStrLn "GENUS FLAGS:"
  putStrLn "  --genus 1   Vélu elliptic isogeny (default, fast)"
  putStrLn "  --genus 2   Richelot (2,2)-isogeny on Genus-2 Jacobian + Siegel modular mixing"

runObfuscate :: ObfuscationConfig -> FilePath -> FilePath -> IO ()
runObfuscate cfg inputFile outputFile = do
  putStrLn $ "[+] Loading Universal Binary: " ++ inputFile
  putStrLn $ "[+] Output Target: " ++ outputFile
  putStrLn $ "[+] Genus: " ++ show (cfgGenus cfg)
  putStrLn ""

  result <- obfuscateFile cfg inputFile outputFile
  case result of
    Left err -> do
      putStrLn $ "✗ Error: " ++ err
    Right od -> do
      putStrLn $ "[✓] Algebraic obfuscation synthesis completed."
      putStrLn $ "  Target File      : " ++ odFilename od
      putStrLn $ "  File Size        : " ++ show (odOrigLen od) ++ " bytes"
      putStrLn $ "  Algebraic Blocks : " ++ show (length (odBlocks od))
      putStrLn $ "  Permission (+x)  : " ++ show (odIsExec od)
      putStrLn $ "  Metadata Blob    : " ++ outputFile ++ ".meta"
      putStrLn ""
      -- Running analysis
      let transforms = odTransforms od
          proof = checkPipelineInvertibility cfg transforms
      putStrLn $ formatProofCompact proof
      let leakage = analyzeLeakage cfg transforms Nothing
      putStrLn $ "Security Score (heuristic): " ++ show (lrSecurityScore leakage) ++ "/100"

runDeobfuscate :: ObfuscationConfig -> FilePath -> FilePath -> IO ()
runDeobfuscate cfg metaFile outputFile = do
  putStrLn $ "[+] Metadata Input: " ++ metaFile
  putStrLn $ "[+] Output Target: " ++ outputFile
  putStrLn ""

  result <- deobfuscateFile cfg metaFile outputFile
  case result of
    Left err -> putStrLn $ "✗ Error: " ++ err
    Right _ -> do
      putStrLn "[✓] Structural recovery completed."
      putStrLn $ "  Recovered Source : " ++ outputFile

runAnalyze :: FilePath -> IO ()
runAnalyze inputFile = do
  let cfg = defaultConfig
  putStrLn $ "[+] Analysis Target: " ++ inputFile
  putStrLn ""
  
  fileBytes <- BS.readFile inputFile
  let len = BS.length fileBytes
  putStrLn $ "  File Size        : " ++ show len ++ " bytes"
  putStrLn $ "  Blocks Needed    : " ++ show (len `div` cfgMaxDegree cfg + 1)
  putStrLn ""

  putStrLn "━━━ Invertibility ━━━"
  let transforms = defaultPipeline cfg
      proof = checkPipelineInvertibility cfg transforms
  putStrLn $ formatProof proof

  putStrLn ""
  putStrLn "━━━ Leakage Vulnerability Analysis ━━━"
  let leakage = analyzeLeakage cfg transforms Nothing
  putStrLn $ formatLeakageReport leakage
