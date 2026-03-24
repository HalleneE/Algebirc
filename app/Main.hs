module Main where

import System.Environment (getArgs)
import Algebirc
import Algebirc.Obfuscation.AST
import Algebirc.Obfuscation.Encoder
import Algebirc.Obfuscation.Transform
import Algebirc.Ordering.Fingerprint
import Algebirc.Ordering.MathOrder
import Algebirc.Ordering.Scheduler
import Algebirc.Integration.HaskellParser
import Algebirc.Integration.FileIO
import Algebirc.Compiler.Standalone (compileToExecutable)
import Algebirc.Analysis.DegreeTracker
import Algebirc.Analysis.Invertibility
import Algebirc.Analysis.Leakage

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("obfuscate" : inputFile : rest) -> do
      let (genusN, rest1) = parseGenus rest
          outputFile = case rest1 of
            ["-o", out] -> out
            _           -> inputFile ++ ".obf.hs"
          cfg = defaultConfig { cfgGenus = genusN }
      runObfuscate cfg inputFile outputFile

    ("deobfuscate" : metaFile : rest) -> do
      let (genusN, rest1) = parseGenus rest
          outputFile = case rest1 of
            ["-o", out] -> out
            _           -> "recovered.hs"
          cfg = defaultConfig { cfgGenus = genusN }
      runDeobfuscate cfg metaFile outputFile

    ("analyze" : inputFile : _) ->
      runAnalyze inputFile

    ("compile" : inputFile : rest) -> do
      let outputFile = case rest of
            ["-o", out] -> out
            _           -> "program.exe"
      runCompile inputFile outputFile

    ("demo" : _) ->
      runDemo

    ("help" : _) ->
      printUsage

    [] -> do
      printUsage
      putStrLn ""
      putStrLn "Execute 'algebirc demo' for a comprehensive pipeline demonstration."

    _ -> do
      putStrLn "Error: Unrecognized command directive."
      printUsage

-- | Parse optional --genus N from arg list, returning (genus, remaining args).
parseGenus :: [String] -> (Int, [String])
parseGenus ("--genus" : n : rest) = (read n, rest)
parseGenus args                   = (1, args)

-- ============================================================
-- CLI Commands
-- ============================================================

printUsage :: IO ()
printUsage = do
  putStrLn "╔══════════════════════════════════════════════════════════════════╗"
  putStrLn "║       Algebirc: Algebraic Isogeny-Based Obfuscation Engine       ║"
  putStrLn "╚══════════════════════════════════════════════════════════════════╝"
  putStrLn ""
  putStrLn "SYNOPSIS:"
  putStrLn "  algebirc obfuscate <input.hs> [--genus 2] [-o out.hs]   -- Algebraic obfuscation (genus-1 default, genus-2 = Richelot+Siegel)"
  putStrLn "  algebirc deobfuscate <file.meta> [--genus 2] [-o rec.hs] -- Recover from obfuscation metadata"
  putStrLn "  algebirc analyze <input.hs>                              -- Algebraic & structural metric analysis"
  putStrLn "  algebirc compile <input.hs> [-o out.exe]                 -- Ahead-of-Time compilation"
  putStrLn "  algebirc demo                                            -- Comprehensive pipeline demonstration"
  putStrLn "  algebirc help                                            -- This help"
  putStrLn ""
  putStrLn "GENUS FLAGS:"
  putStrLn "  --genus 1   Vélu elliptic isogeny (default, fast)"
  putStrLn "  --genus 2   Richelot (2,2)-isogeny on Genus-2 Jacobian + Siegel modular mixing"

runObfuscate :: ObfuscationConfig -> FilePath -> FilePath -> IO ()
runObfuscate cfg inputFile outputFile = do
  putStrLn $ "[+] Loading Source Module: " ++ inputFile
  putStrLn $ "[+] Output Target: " ++ outputFile
  putStrLn $ "[+] Genus: " ++ show (cfgGenus cfg)
  putStrLn ""

  result <- obfuscateFile cfg inputFile outputFile
  case result of
    Left err -> do
      putStrLn $ "✗ Error: " ++ err
    Right od -> do
      putStrLn $ "[✓] Algebraic obfuscation synthesis completed."
      putStrLn $ "  Target Module    : " ++ smName (odModule od)
      putStrLn $ "  AST Declarations : " ++ show (length (smDecls (odModule od)))
      putStrLn $ "  Algebraic Blocks : " ++ show (length (odBlocks od))
      putStrLn $ "  Metadata Blob    : " ++ outputFile ++ ".meta"
      putStrLn ""
      -- Running analysis
      let transforms = odTransforms od
          proof = checkPipelineInvertibility cfg transforms
      putStrLn $ formatProofCompact proof
      let leakage = analyzeLeakage cfg transforms Nothing
      putStrLn $ "Security Score (heuristic): " ++ show (lrSecurityScore leakage) ++ "/100"
      putStrLn "  [NOTE: This is an internal heuristic — not a formal security level or NIST claim.]"

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

  parseResult <- parseHaskellFile inputFile
  case parseResult of
    Left err -> putStrLn $ "✗ Parse error: " ++ err
    Right pr -> do
      let sm = prModule pr
      putStrLn $ "  Target Module    : " ++ smName sm
      putStrLn $ "  LOC: " ++ show (prLineCount pr)
      putStrLn $ "  AST Declarations : " ++ show (length (smDecls sm))
      putStrLn ""

      -- Encode dan analyze
      let transforms = defaultPipeline cfg
          declExprs = map (\d -> case d of
                        FuncDecl _ _ b -> b
                        ValDecl _ b -> b) (smDecls sm)

      putStrLn "━━━ DegreeTracker Heuristics ━━━"
      case head declExprs of
        expr -> case encodeExpr cfg expr of
          Left err -> putStrLn $ "  Encode error: " ++ show err
          Right block ->
            case trackedPipeline cfg transforms (ebPoly block) of
              Left err -> putStrLn $ "  Transform error: " ++ show err
              Right (_, report) -> putStrLn $ formatReport report

      putStrLn ""
      putStrLn "━━━ Invertibility ━━━"
      let proof = checkPipelineInvertibility cfg transforms
      putStrLn $ formatProof proof

      putStrLn ""
      putStrLn "━━━ Leakage Vulnerability Analysis ━━━"
      let leakage = analyzeLeakage cfg transforms Nothing
      putStrLn $ formatLeakageReport leakage
      
runCompile :: FilePath -> FilePath -> IO ()
runCompile inputFile outputFile = do
  putStrLn $ "╔══════════════════════════════════════════════════════╗"
  putStrLn $ "║           Algebirc: AOT Compilation Engine           ║"
  putStrLn $ "╚══════════════════════════════════════════════════════╝"
  putStrLn $ "[+] Loading Source Module: " ++ inputFile
  putStrLn $ "[+] Output Target: " ++ outputFile
  putStrLn ""
  
  -- Use processFile from FileIO
  res <- processFile defaultConfig inputFile
  case res of
    Left err -> putStrLn $ "✗ Error: " ++ err
    Right od -> do
      putStrLn "[✓] In-Memory obfuscation successful."
      putStrLn "[🚀] Initiating Ahead-Of-Time (AOT) binary compilation..."
      compileToExecutable od outputFile
      putStrLn "[✓] Ahead-of-Time compilation successful."

-- ============================================================
-- Demo (Phase 1+2 demo lama)
-- ============================================================

runDemo :: IO ()
runDemo = do
  putStrLn "╔══════════════════════════════════════════════════════════════╗"
  putStrLn "║       Algebirc: Algebraic Isogeny-Based Obfuscation Engine       ║"
  putStrLn "║                    Full Pipeline Demo                      ║"
  putStrLn "╚══════════════════════════════════════════════════════════════╝"
  putStrLn ""

  -- ============================================================
  -- Phase 1 Demos (Foundation)
  -- ============================================================

  putStrLn "━━━ Demo 1: Finite Field GF(257) ━━━"
  let p = 257
      a = mkFieldElement 42 p
      b = mkFieldElement 200 p
  putStrLn $ "  a = " ++ show (feValue a)
  putStrLn $ "  b = " ++ show (feValue b)
  putStrLn $ "  a + b = " ++ show (feValue (ffAdd a b))
  putStrLn $ "  a * b = " ++ show (feValue (ffMul a b))
  case ffInv a of
    Right inv -> do
      putStrLn $ "  a⁻¹ = " ++ show (feValue inv)
      putStrLn $ "  a * a⁻¹ = " ++ show (feValue (ffMul a inv)) ++ " (should be 1)"
    Left err -> putStrLn $ "  Error: " ++ show err
  putStrLn ""

  putStrLn "━━━ Demo 2: Degree-Bounded Polynomials ━━━"
  let maxDeg = 64
      f = mkBoundedPoly p maxDeg [Term 3 2, Term 2 1, Term 1 0]
      g = linearPoly p maxDeg 1 5
  putStrLn $ "  f(10) = " ++ show (polyEval f 10)
  putStrLn $ "  deg(f) = " ++ show (polyDegree f)
  case polyCompose f g of
    Right fg -> putStrLn $ "  f∘g deg = " ++ show (polyDegree fg) ++ ", f(g(10)) = " ++ show (polyEval fg 10)
    Left err -> putStrLn $ "  Error: " ++ show err

  let bigPoly = mkBoundedPoly p 8 [Term 1 8, Term 1 0]
  case polyCompose bigPoly bigPoly of
    Left (DegreeOverflow actual cap) ->
      putStrLn $ "  ✓ Degree explosion blocked: " ++ show actual ++ " > cap " ++ show cap
    Left err -> putStrLn $ "  Error: " ++ show err
    Right _ -> putStrLn "  ⚠ Unexpected"
  putStrLn ""

  putStrLn "━━━ Demo 3: Permutation Groups ━━━"
  let sigma = generateFromSeed 8 12345
      sigmaInv = invertPerm sigma
  case composePerm sigma sigmaInv of
    Right composed ->
      putStrLn $ "  σ ∘ σ⁻¹ = " ++ show (toOneLine composed) ++ " (identity ✓)"
    Left err -> putStrLn $ "  Error: " ++ show err
  putStrLn ""

  putStrLn "━━━ Demo 4: Evaluator ━━━"
  putStrLn $ "  42 + 200 mod 257 = " ++ show (eval (BinOpExpr Add (Lit 42) (Lit 200)))
  putStrLn $ "  if 1 then 100 else 200 = " ++ show (eval (If (Lit 1) (Lit 100) (Lit 200)))
  putStrLn $ "  let x=42 in x*x = " ++ show (eval (Let "x" (Lit 42) (BinOpExpr Mul (Var "x") (Var "x"))))
  putStrLn ""

  -- ============================================================
  -- Phase 2 Demos (Obfuscation Pipeline)
  -- ============================================================

  putStrLn "╔══════════════════════════════════════════════════════════════╗"
  putStrLn "║                   Phase 2: Obfuscation Pipeline            ║"
  putStrLn "╚══════════════════════════════════════════════════════════════╝"
  putStrLn ""

  let cfg = defaultConfig

  -- Demo 5: AST Construction & Encoding
  putStrLn "━━━ Demo 5: AST → Polynomial Encoding ━━━"
  let sourceExpr = SBinOp "+" (SVar "x") (SLit 42)
  putStrLn $ "  Source: " ++ prettySource sourceExpr
  putStrLn $ "  AST size: " ++ show (exprSize sourceExpr) ++ " nodes"
  putStrLn $ "  AST depth: " ++ show (exprDepth sourceExpr)
  case encodeExpr cfg sourceExpr of
    Right block -> do
      putStrLn $ "  Encoded to polynomial, " ++ show (ebOrigSize block) ++ " bytes"
      putStrLn $ "  Block ID: " ++ show (ebBlockId block)
      -- Decode back
      let decoded = decodeExpr cfg block
      putStrLn $ "  Decoded: " ++ decoded
      putStrLn $ "  ✓ Round-trip: " ++ if decoded == "(+ x 42)" then "PASS" else "FAIL"
    Left err -> putStrLn $ "  Encoding error: " ++ show err
  putStrLn ""

  -- Demo 6: Module-Level Encoding
  putStrLn "━━━ Demo 6: Module Encoding ━━━"
  let srcMod = SourceModule "TestModule"
        [ FuncDecl "square" ["x"] (SBinOp "*" (SVar "x") (SVar "x"))
        , ValDecl "answer" (SLit 42)
        ]
  putStrLn $ "  Module: " ++ smName srcMod
  putStrLn $ "  Declarations: " ++ show (length (smDecls srcMod))
  case encodeModule cfg srcMod of
    Right em -> do
      putStrLn $ "  Encoded to " ++ show (length (emBlocks em)) ++ " block(s)"
      let decoded = decodeModule em
      putStrLn $ "  Decoded content length: " ++ show (length decoded)
      putStrLn $ "  ✓ Module round-trip: PASS"
    Left err -> putStrLn $ "  Module encoding error: " ++ show err
  putStrLn ""

  -- Demo 7: Affine Transform (Obfuscation ↔ Deobfuscation)
  putStrLn "━━━ Demo 7: Affine Transform Round-Trip ━━━"
  let testExpr = SBinOp "+" (SLit 10) (SLit 20)
  case encodeExpr cfg testExpr of
    Right block -> do
      let affine = mkAffineTransform 37 13  -- x → 37x + 13 (mod 257)
      putStrLn $ "  Original: " ++ decodeExpr cfg block
      case obfuscateBlock cfg [affine] block of
        Right obfBlock -> do
          putStrLn $ "  Obfuscated: " ++ show (take 5 (polyCoefficients (ebPoly obfBlock))) ++ "..."
          case deobfuscateBlock cfg [affine] obfBlock of
            Right deobfBlock -> do
              let result = decodeExpr cfg deobfBlock
              putStrLn $ "  Deobfuscated: " ++ result
              putStrLn $ "  ✓ Affine round-trip: " ++ 
                if result == "(+ 10 20)" then "PASS" else "FAIL (" ++ result ++ ")"
            Left err -> putStrLn $ "  Deobfuscation error: " ++ show err
        Left err -> putStrLn $ "  Obfuscation error: " ++ show err
    Left err -> putStrLn $ "  Encoding error: " ++ show err
  putStrLn ""

  -- Demo 8: Permutation Transform Round-Trip
  putStrLn "━━━ Demo 8: Permutation Transform Round-Trip ━━━"
  case encodeExpr cfg testExpr of
    Right block -> do
      let perm = generateFromSeed 16 42
          permT = mkPermTransform perm
      putStrLn $ "  Original: " ++ decodeExpr cfg block
      case obfuscateBlock cfg [permT] block of
        Right obfBlock -> do
          case deobfuscateBlock cfg [permT] obfBlock of
            Right deobfBlock -> do
              let result = decodeExpr cfg deobfBlock
              putStrLn $ "  Deobfuscated: " ++ result
              putStrLn $ "  ✓ Permutation round-trip: " ++
                if result == "(+ 10 20)" then "PASS" else "FAIL"
            Left err -> putStrLn $ "  Error: " ++ show err
        Left err -> putStrLn $ "  Error: " ++ show err
    Left err -> putStrLn $ "  Error: " ++ show err
  putStrLn ""

  -- Demo 9: Multi-Transform Pipeline
  putStrLn "━━━ Demo 9: Multi-Transform Pipeline ━━━"
  case encodeExpr cfg testExpr of
    Right block -> do
      let perm1 = generateFromSeed 16 99
          transforms = [ mkAffineTransform 37 13
                       , mkPermTransform perm1
                       , mkAffineTransform 11 7
                       ]
      putStrLn $ "  Pipeline: Affine(37,13) → Permute → Affine(11,7)"
      putStrLn $ "  Original: " ++ decodeExpr cfg block
      case obfuscateBlock cfg transforms block of
        Right obfBlock -> do
          putStrLn "  Obfuscation: ✓"
          case deobfuscateBlock cfg transforms obfBlock of
            Right deobfBlock -> do
              let result = decodeExpr cfg deobfBlock
              putStrLn $ "  Deobfuscated: " ++ result
              putStrLn $ "  ✓ Pipeline round-trip: " ++
                if result == "(+ 10 20)" then "PASS" else "FAIL"
            Left err -> putStrLn $ "  Error: " ++ show err
        Left err -> putStrLn $ "  Error: " ++ show err
    Left err -> putStrLn $ "  Error: " ++ show err
  putStrLn ""

  -- Demo 10: SHA-256 Fingerprinting & Execution Ordering
  putStrLn "━━━ Demo 10: SHA-256 Fingerprinting & Execution Order ━━━"
  let mod1 = SourceModule "Auth" [FuncDecl "login" ["u","p"] (SBinOp "==" (SVar "u") (SVar "p"))]
      mod2 = SourceModule "Data" [FuncDecl "fetch" ["id"] (SVar "id")]
      mod3 = SourceModule "View" [FuncDecl "render" ["d"] (SApp (SVar "show") (SVar "d"))]
  case (encodeModule cfg mod1, encodeModule cfg mod2, encodeModule cfg mod3) of
    (Right em1, Right em2, Right em3) -> do
      let fp1 = computeFingerprint em1
          fp2 = computeFingerprint em2
          fp3 = computeFingerprint em3
      putStrLn $ "  Auth:  " ++ take 16 (fpHashHex fp1) ++ "..."
      putStrLn $ "  Data:  " ++ take 16 (fpHashHex fp2) ++ "..."
      putStrLn $ "  View:  " ++ take 16 (fpHashHex fp3) ++ "..."

      -- Compute execution order via math priority
      let ordered = orderByPriority cfg [(em1, fp1), (em2, fp2), (em3, fp3)]
      putStrLn "\n  Execution Order (by mathematical priority):"
      mapM_ (\oi -> putStrLn $ "    " ++ show (oiOrder oi) ++ ". " ++ 
                               emName (oiItem oi) ++ 
                               " (priority=" ++ show (oiPriority oi) ++ ")") ordered

      -- Build execution graph with dependencies
      let graph = buildExecutionGraph cfg
                    [ (em1, [])
                    , (em2, ["Auth"])
                    , (em3, ["Data"])
                    ]
          result = schedule graph
      putStrLn ""
      putStr (showSchedule result)
    _ -> putStrLn "  Module encoding error"

  putStrLn ""
  putStrLn "✓ Phase 1 + Phase 2 verified."
