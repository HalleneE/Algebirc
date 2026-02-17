module Algebirc.Compiler.Standalone
  ( compileToExecutable
  ) where

import Algebirc.Core.Types
import Algebirc.Integration.FileIO (ObfuscationData(..))
import Algebirc.Compiler.Metamorphic (obfuscateConstant)
import Algebirc.Obfuscation.Encoder (EncodedBlock(..))
import Algebirc.Core.Polynomial

import System.Directory
import System.FilePath
import System.Process (callProcess)
import System.Random (newStdGen, RandomGen)
import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map  -- used by caller

-- | Compile obfuscated data into a standalone executable.
compileToExecutable :: ObfuscationData -> FilePath -> IO ()
compileToExecutable od _outputPath = do
  -- 1. Create temp directory
  tmpDir <- getTemporaryDirectory
  let buildDir = tmpDir </> "algebirc_build_" ++ show (cfgSeed (odConfig od))
  createDirectoryIfMissing True buildDir
  
  putStrLn $ "Building standalone runtime in: " ++ buildDir

  -- 2. Copy dependencies (Core + Obfuscation subset)
  let modules = 
        [ "Algebirc/Core/Types.hs"
        , "Algebirc/Core/FiniteField.hs"
        , "Algebirc/Core/Polynomial.hs"
        , "Algebirc/Obfuscation/AST.hs"
        , "Algebirc/Obfuscation/Encoder.hs"
        ]
  
  forM_ modules $ \modPath -> do
    let srcPath = "src" </> modPath
    exists <- doesFileExist srcPath
    if exists
      then do
        let dstPath = buildDir </> modPath
        createDirectoryIfMissing True (takeDirectory dstPath)
        copyFile srcPath dstPath
      else putStrLn $ "WARNING: Source file not found: " ++ srcPath

  -- 3. Generate Main.hs with full runtime engine
  gen <- newStdGen
  mainContent <- generateMainHs od gen
  writeFile (buildDir </> "Main.hs") mainContent

  -- 3.5 Generate runtime.cabal
  let cabalContent = unlines
        [ "cabal-version: 3.0"
        , "name: algebirc-runtime"
        , "version: 0.1.0.0"
        , "executable algebirc-runtime"
        , "  main-is: Main.hs"
        , "  hs-source-dirs: ."
        , "  other-modules:"
        , "    Algebirc.Core.Types"
        , "    Algebirc.Core.FiniteField"
        , "    Algebirc.Core.Polynomial"
        , "    Algebirc.Obfuscation.AST"
        , "    Algebirc.Obfuscation.Encoder"
        , "  build-depends:"
        , "    base >= 4.17 && < 5,"
        , "    vector,"
        , "    deepseq,"
        , "    cryptonite,"
        , "    memory,"
        , "    aeson,"
        , "    text,"
        , "    bytestring,"
        , "    containers,"
        , "    mtl,"
        , "    random"
        , "  default-language: Haskell2010"
        , "  default-extensions:"
        , "    ScopedTypeVariables"
        , "    DeriveGeneric"
        , "    DeriveAnyClass"
        , "    OverloadedStrings"
        , "    StrictData"
        ]
  writeFile (buildDir </> "algebirc-runtime.cabal") cabalContent

  -- 4. Compile with Cabal
  putStrLn "Compiling with Cabal..."
  currentDir <- getCurrentDirectory
  setCurrentDirectory buildDir
  callProcess "cabal" ["build"]
  setCurrentDirectory currentDir

  putStrLn $ "Compilation complete. Artifacts in " ++ buildDir

-- ============================================================
-- Main.hs Code Generator (Full Runtime Engine)
-- ============================================================

-- | Generate Main.hs with full encode → eval → decode pipeline.
generateMainHs :: RandomGen g => ObfuscationData -> g -> IO String
generateMainHs od gen = do
  let blocks = map snd (odBlocks od) -- EncodedBlock (obfuscated)
      cfg    = odConfig od
      p      = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      (blockCode, _) = generateBlockData blocks gen
      
      -- Extract origSize from each block for the size map
      sizeMap = map (\(_, b) -> (ebBlockId b, ebOrigSize b)) (odBlocks od)
      sizeMapStr = "[" ++ intercalate "," 
                    (map (\(bid,sz) -> "(" ++ show bid ++ "," ++ show sz ++ ")") sizeMap) 
                   ++ "]"
      
      -- Extract transform chain info for embedding
      transforms = odTransforms od
      transformInfoStr = show (length transforms)
      
      -- Helper: quote a string for embedding in generated Haskell
      q s = "\"" ++ s ++ "\""
  
  return $ unlines $
    -- Module header and imports
    [ "module Main where"
    , ""
    , "import Algebirc.Core.Types"
    , "import Algebirc.Core.FiniteField"
    , "import Algebirc.Core.Polynomial"
    , "import Algebirc.Obfuscation.Encoder"
    , "import Algebirc.Obfuscation.AST"
    , "import qualified Data.Map.Strict as Map"
    , "import Data.List (foldl', intercalate)"
    , "import Data.Char (chr, ord, toUpper)"
    , "import System.Environment (getArgs)"
    , "import Data.Bits (xor)"
    , "import Numeric (showHex)"
    , "import System.CPUTime (getCPUTime)"
    , ""
    -- Configuration
    , "runtimePrime :: Integer"
    , "runtimePrime = " ++ show p
    , ""
    , "runtimeMaxDeg :: Int"
    , "runtimeMaxDeg = " ++ show maxDeg
    , ""
    , "runtimeTransformDepth :: Int"
    , "runtimeTransformDepth = " ++ transformInfoStr
    , ""
    -- Metamorphic Data
    , "obfuscatedBlocks :: [(Int, BoundedPoly)]"
    , "obfuscatedBlocks = "
    , "  " ++ blockCode
    , ""
    , "blockSizeMap :: [(Int, Int)]"
    , "blockSizeMap = " ++ sizeMapStr
    ] ++
    -- Self-contained runtime engine (no string-in-string issues)
    runtimeEngineCode

-- | The runtime engine code as plain Haskell lines.
-- These are literal strings that produce valid Haskell when joined with newlines.
runtimeEngineCode :: [String]
runtimeEngineCode =
  [ ""
  , "-- ========================================================"
  , "-- SELF-CONTAINED POLYNOMIAL EVALUATOR"
  , "-- ========================================================"
  , ""
  , "evalPolyAt :: Integer -> BoundedPoly -> Integer -> Integer"
  , "evalPolyAt p poly x ="
  , "  let coeffs = [ (termCoeff t, termExp t) | t <- polyTerms poly ]"
  , "  in foldl' (\\acc (c, e) -> (acc + c * powMod x (fromIntegral e) p) `mod` p) 0 coeffs"
  , ""
  , "powMod :: Integer -> Integer -> Integer -> Integer"
  , "powMod _ 0 _ = 1"
  , "powMod base' e m"
  , "  | even e    = let half = powMod base' (e `div` 2) m in (half * half) `mod` m"
  , "  | otherwise = (base' * powMod base' (e - 1) m) `mod` m"
  , ""
  , "-- ========================================================"
  , "-- ARX DIFFUSION + FEISTEL ENGINE"
  , "-- ========================================================"
  , ""
  , "runtimeSBox :: Integer -> Integer -> Integer"
  , "runtimeSBox p x"
  , "  | x == 0    = (37 * p `div` 7 + 19) `mod` p"
  , "  | otherwise = let inv = powMod x (p - 2) p"
  , "                in (inv * 137 + 42) `mod` p"
  , ""
  , "arxDiffuse :: Integer -> [Integer] -> [Integer]"
  , "arxDiffuse _ [] = []"
  , "arxDiffuse _ [x] = [x]"
  , "arxDiffuse p xs ="
  , "  let n = length xs"
  , "      fwd = scanl1 (\\prev cur -> (cur + runtimeSBox p prev + 7) `mod` p) xs"
  , "      bwd = reverse $ scanl1 (\\prev cur -> (cur + runtimeSBox p prev + 13) `mod` p) (reverse fwd)"
  , "      mixed = [ let j = (i + n `div` 3) `mod` n"
  , "                    val = bwd !! i"
  , "                    partner = bwd !! j"
  , "                in (runtimeSBox p (val + partner) + fromIntegral i) `mod` p"
  , "              | i <- [0..n-1] ]"
  , "  in mixed"
  , ""
  , "feistelEncode :: Integer -> [Integer] -> [Integer]"
  , "feistelEncode _ [] = []"
  , "feistelEncode _ [x] = [x]"
  , "feistelEncode p xs ="
  , "  let pairs = pairUp xs"
  , "      processed = map (feistelPairFwd p 4) pairs"
  , "  in concatMap (\\(l,r) -> [l,r]) processed"
  , ""
  , "pairUp :: [a] -> [(a, a)]"
  , "pairUp [] = []"
  , "pairUp [x] = [(x, x)]"
  , "pairUp (a:b:rest) = (a, b) : pairUp rest"
  , ""
  , "feistelPairFwd :: Integer -> Int -> (Integer, Integer) -> (Integer, Integer)"
  , "feistelPairFwd p rounds (l0, r0) ="
  , "  foldl' (\\(l, r) i ->"
  , "    let roundKey = (fromIntegral i * 6364136223846793005 + 1442695040888963407) `mod` p"
  , "        f = runtimeSBox p ((r + roundKey) `mod` p)"
  , "        l' = (l + f) `mod` p"
  , "    in (r, l')"
  , "  ) (l0, r0) [1..rounds]"
  , ""
  , "-- ========================================================"
  , "-- INPUT ENCODER + DECODER"
  , "-- ========================================================"
  , ""
  , "encodeInput :: Integer -> String -> [Integer]"
  , "encodeInput p input ="
  , "  let bytes = map (fromIntegral . ord) input"
  , "      fieldElems = map (`mod` p) bytes"
  , "      diffused = arxDiffuse p fieldElems"
  , "      feistelled = feistelEncode p diffused"
  , "  in feistelled"
  , ""
  , "extractBytes :: BoundedPoly -> Int -> [Integer]"
  , "extractBytes poly n = [ getCoeffAt i poly | i <- [0..n-1] ]"
  , ""
  , "decodePoly :: BoundedPoly -> Int -> String"
  , "decodePoly poly n ="
  , "  let bytes = extractBytes poly n"
  , "  in map (chr . fromIntegral . (`mod` 256)) bytes"
  , ""
  , "-- ========================================================"
  , "-- OUTPUT FORMATTING"
  , "-- ========================================================"
  , ""
  , "toHexStr :: Integer -> String"
  , "toHexStr n"
  , "  | n < 0     = \"-\" ++ toHexStr (abs n)"
  , "  | n == 0    = \"00\""
  , "  | otherwise = let hex = showHex n \"\""
  , "                    padded = if length hex `mod` 2 == 1 then '0':hex else hex"
  , "                in map toUpper padded"
  , ""
  , "hexDump :: [Integer] -> String"
  , "hexDump = intercalate \" \" . map toHexStr"
  , ""
  , "sep :: String -> String"
  , "sep label = \"\\n\" ++ replicate 60 '=' ++ \"\\n  \" ++ label ++ \"\\n\" ++ replicate 60 '='"
  , ""
  , "-- ========================================================"
  , "-- MAIN RUNTIME ENGINE"
  , "-- ========================================================"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  args <- getArgs"
  , "  case args of"
  , ""
  , "    (\"eval\":blockIdStr:inputs) -> do"
  , "      let blockId = read blockIdStr :: Int"
  , "      case lookup blockId obfuscatedBlocks of"
  , "        Nothing -> putStrLn $ \"ERROR: Block ID \" ++ show blockId ++ \" not found.\""
  , "        Just poly -> do"
  , "          t0 <- getCPUTime"
  , "          putStrLn $ sep \"ALGEBIRC RUNTIME v2.0 | EVAL MODE\""
  , "          putStrLn $ \"  Block ID       : \" ++ show blockId"
  , "          putStrLn $ \"  Field Prime    : \" ++ show runtimePrime"
  , "          putStrLn $ \"  Max Degree     : \" ++ show runtimeMaxDeg"
  , "          putStrLn $ \"  Transform Depth: \" ++ show runtimeTransformDepth"
  , "          putStrLn $ \"  Poly Terms     : \" ++ show (length (polyTerms poly))"
  , "          let inputStr = unwords inputs"
  , "          putStrLn $ sep \"STAGE 1: INPUT ENCODING\""
  , "          putStrLn $ \"  Raw Input      : \" ++ show inputStr"
  , "          let encoded = encodeInput runtimePrime inputStr"
  , "          putStrLn $ \"  Field Elems    : \" ++ show (take 16 encoded)"
  , "          putStrLn $ \"  Hex Encoded    : \" ++ hexDump (take 16 encoded)"
  , "          putStrLn $ sep \"STAGE 2: ARX DIFFUSION + FEISTEL\""
  , "          putStrLn $ \"  Diffusion      : 3-pass (fwd + bwd + cross-mix)\""
  , "          putStrLn $ \"  Feistel        : 4-round balanced network\""
  , "          putStrLn $ \"  S-Box          : Fermat inverse + affine post-mix\""
  , "          putStrLn $ sep \"STAGE 3: POLYNOMIAL EVALUATION GF(p)\""
  , "          let results = [ evalPolyAt runtimePrime poly x | x <- encoded ]"
  , "          putStrLn $ \"  Input Points   : \" ++ show (length encoded)"
  , "          putStrLn $ \"  Output Values  : \" ++ show (take 16 results)"
  , "          putStrLn $ \"  Hex Output     : \" ++ hexDump (take 16 results)"
  , "          t1 <- getCPUTime"
  , "          let elapsed = fromIntegral (t1 - t0) / (10^12 :: Double)"
  , "          putStrLn $ sep \"RESULT\""
  , "          putStrLn $ \"  Output count   : \" ++ show (length results)"
  , "          putStrLn $ \"  Time           : \" ++ show elapsed ++ \"s\""
  , "          putStrLn   \"  Pipeline       : Input -> ARX -> Feistel -> Poly(GF(p)) -> Output\""
  , "          putStrLn   \"  Status         : SUCCESS\""
  , ""
  , "    (\"decode\":blockIdStr:_) -> do"
  , "      let blockId = read blockIdStr :: Int"
  , "      case lookup blockId obfuscatedBlocks of"
  , "        Nothing -> putStrLn $ \"ERROR: Block ID \" ++ show blockId ++ \" not found.\""
  , "        Just poly -> do"
  , "          putStrLn $ sep \"ALGEBIRC RUNTIME v2.0 | DECODE MODE\""
  , "          putStrLn $ \"  Block ID       : \" ++ show blockId"
  , "          let origSize = case lookup blockId blockSizeMap of"
  , "                           Just s  -> s"
  , "                           Nothing -> polyMaxDeg poly + 1"
  , "          putStrLn $ \"  Original Size  : \" ++ show origSize ++ \" bytes\""
  , "          putStrLn $ sep \"COEFFICIENT EXTRACTION\""
  , "          let bytes = extractBytes poly origSize"
  , "          putStrLn $ \"  Raw Coefficients: \" ++ show (take 32 bytes)"
  , "          putStrLn $ \"  Hex Dump        : \" ++ hexDump (take 32 bytes)"
  , "          putStrLn $ sep \"DECODED DATA\""
  , "          let decoded = decodePoly poly origSize"
  , "          putStrLn $ \"  Decoded String : \" ++ show decoded"
  , "          putStrLn $ \"  Length         : \" ++ show (length decoded) ++ \" chars\""
  , "          putStrLn   \"  Status         : SUCCESS\""
  , ""
  , "    [\"info\"] -> do"
  , "      putStrLn $ sep \"ALGEBIRC RUNTIME v2.0 | INFO\""
  , "      putStrLn $ \"  Blocks Embedded  : \" ++ show (length obfuscatedBlocks)"
  , "      putStrLn $ \"  Field Prime (p)  : \" ++ show runtimePrime"
  , "      putStrLn $ \"  Max Degree       : \" ++ show runtimeMaxDeg"
  , "      putStrLn $ \"  Transform Depth  : \" ++ show runtimeTransformDepth"
  , "      putStrLn   \"  Obfuscation Layers:\""
  , "      putStrLn   \"    - Metamorphic Constants (compile-time)\""
  , "      putStrLn   \"    - ARX Diffusion (3-pass, runtime)\""
  , "      putStrLn   \"    - Feistel Network (4-round, runtime)\""
  , "      putStrLn $ \"    - Polynomial Field GF(\" ++ show runtimePrime ++ \") (runtime)\""
  , "      putStrLn   \"\""
  , "      putStrLn   \"  Block Details:\""
  , "      mapM_ (\\(bid, poly) ->"
  , "        putStrLn $ \"    Block \" ++ show bid ++ \": \""
  , "                  ++ show (length (polyTerms poly)) ++ \" terms, \""
  , "                  ++ \"deg=\" ++ show (polyMaxDeg poly)"
  , "                  ++ \" | GF(\" ++ show (polyField poly) ++ \")\""
  , "        ) obfuscatedBlocks"
  , "      putStrLn   \"\\n  Engine: Algebirc Metamorphic Runtime v2.0\""
  , ""
  , "    (\"batch\":inputs) -> do"
  , "      let inputStr = unwords inputs"
  , "      t0 <- getCPUTime"
  , "      putStrLn $ sep \"ALGEBIRC RUNTIME v2.0 | BATCH MODE\""
  , "      putStrLn $ \"  Input: \" ++ show inputStr"
  , "      putStrLn $ \"  Processing \" ++ show (length obfuscatedBlocks) ++ \" blocks...\""
  , "      putStrLn \"\""
  , "      let encoded = encodeInput runtimePrime inputStr"
  , "      mapM_ (\\(bid, poly) -> do"
  , "        let results = [ evalPolyAt runtimePrime poly x | x <- encoded ]"
  , "        putStrLn $ \"  Block \" ++ show bid ++ \": \" ++ hexDump (take 8 results)"
  , "        ) obfuscatedBlocks"
  , "      t1 <- getCPUTime"
  , "      let elapsed = fromIntegral (t1 - t0) / (10^12 :: Double)"
  , "      putStrLn $ \"\\n  Total time: \" ++ show elapsed ++ \"s\""
  , "      putStrLn   \"  Status: SUCCESS\""
  , ""
  , "    _ -> do"
  , "      putStrLn $ sep \"ALGEBIRC METAMORPHIC RUNTIME v2.0\""
  , "      putStrLn   \"  Usage:\""
  , "      putStrLn   \"    program eval   <block_id> <input...>  Encode + Eval polynomial\""
  , "      putStrLn   \"    program decode  <block_id>            Decode embedded data\""
  , "      putStrLn   \"    program batch   <input...>            Eval all blocks\""
  , "      putStrLn   \"    program info                          Show runtime info\""
  , "      putStrLn   \"\""
  , "      putStrLn $ \"  Blocks: \" ++ show (map fst obfuscatedBlocks)"
  , "      putStrLn $ \"  Field: GF(\" ++ show runtimePrime ++ \")  |  MaxDeg: \" ++ show runtimeMaxDeg"
  ]

-- ============================================================
-- Block Data Generator (Metamorphic Constants)
-- ============================================================

-- | Generate Haskell code list for blocks using metamorphic constants.
generateBlockData :: RandomGen g => [EncodedBlock] -> g -> (String, g)
generateBlockData blocks gen = 
  let (elems, gFinal) = foldl step ([], gen) blocks
      step (acc, g) block = 
        let (s, g') = formatBlock block g
        in (acc ++ [s], g')
  in ("[" ++ intercalate "," elems ++ "]", gFinal)

formatBlock :: RandomGen g => EncodedBlock -> g -> (String, g)
formatBlock b g =
  let poly = ebPoly b
      terms = polyTerms poly
      (termStrings, gFinal) = foldl step ([], g) terms
      
      step (acc, rng) (Term coeff deg) =
        let (valStr, rng') = obfuscateConstant coeff rng
        in (acc ++ ["(Term " ++ valStr ++ " " ++ show deg ++ ")"], rng')
      
      optTermStr = "[" ++ intercalate "," termStrings ++ "]"
      
      -- Reconstruct BoundedPoly constructor code matching Types.hs
      polyCode = "BoundedPoly " ++ optTermStr ++ " " 
                 ++ show (polyMaxDeg poly) ++ " " ++ show (polyField poly)
                 
  in ("(" ++ show (ebBlockId b) ++ ", " ++ polyCode ++ ")", gFinal)
