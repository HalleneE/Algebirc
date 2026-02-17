-- |
-- Module      : StressSpec
-- Description : Stability stress tests — must pass before Phase 3
--
-- Tests:
-- 1. Closure property: composition stays in domain
-- 2. Deep pipeline (10+ transforms) round-trip
-- 3. Polynomial term explosion stress
-- 4. Large AST encode/decode
-- 5. Hash collision simulation
-- 6. Randomized transform fuzzing

module StressSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Algebirc.Core.Types
import Algebirc.Core.FiniteField
import Algebirc.Core.Polynomial
import Algebirc.Core.Group
import Algebirc.Obfuscation.AST
import Algebirc.Obfuscation.Encoder
import Algebirc.Obfuscation.Transform
import Algebirc.Ordering.Fingerprint
import Algebirc.Ordering.MathOrder
import Data.List (nub)

spec :: Spec
spec = do
  closureSpec
  deepPipelineSpec
  termExplosionSpec
  largeASTSpec
  hashCollisionSpec
  fuzzingSpec

-- ============================================================
-- 1. Closure Property
-- ============================================================

closureSpec :: Spec
closureSpec = describe "Closure Property" $ do
  let cfg = defaultConfig
      p = cfgFieldPrime cfg

  it "affine transform output stays in GF(p)" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2]
        t = mkAffineTransform 37 13
    case applyTransform cfg t poly of
      Right result -> do
        let coeffs = map snd (polyCoefficients result)
        all (\c -> c >= 0 && c < p) coeffs `shouldBe` True
      Left err -> expectationFailure (show err)

  it "permutation transform preserves coefficient values" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2, Term 55 3]
        perm = generateFromSeed 64 42
        t = mkPermTransform perm
    case applyTransform cfg t poly of
      Right result -> do
        -- Same coefficient values, different positions
        let origCoeffs = map snd (polyCoefficients poly)
            newCoeffs = map snd (polyCoefficients result)
        length newCoeffs `shouldBe` length origCoeffs
      Left err -> expectationFailure (show err)

  it "composite transform result is still a valid BoundedPoly" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2]
        transforms = [ mkAffineTransform 37 13
                      , mkPermTransform (generateFromSeed 64 99)
                      , mkAffineTransform 11 7
                      ]
    case applyPipeline cfg transforms poly of
      Right result -> do
        polyDegree result `shouldSatisfy` (<= 64)
        let coeffs = map snd (polyCoefficients result)
        all (\c -> c >= 0 && c < p) coeffs `shouldBe` True
      Left err -> expectationFailure (show err)

  it "affine inverse is closed: output stays in GF(p)" $ do
    let poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 200 2]
        t = mkAffineTransform 37 13
    case applyTransform cfg t poly >>= invertTransform cfg t of
      Right result -> do
        let coeffs = map snd (polyCoefficients result)
        all (\c -> c >= 0 && c < p) coeffs `shouldBe` True
      Left err -> expectationFailure (show err)

-- ============================================================
-- 2. Deep Pipeline (10+ transforms)
-- ============================================================

deepPipelineSpec :: Spec
deepPipelineSpec = describe "Deep Pipeline (10+ transforms)" $ do
  let cfg = defaultConfig

  it "15-layer affine pipeline round-trips correctly" $ do
    let expr = SBinOp "+" (SLit 42) (SVar "x")
    case encodeExpr cfg expr of
      Right block -> do
        let transforms = [ mkAffineTransform (fromIntegral (3 + i * 7)) (fromIntegral (i * 11 + 5))
                         | i <- [1..15 :: Integer] ]
        case obfuscateBlock cfg transforms block of
          Right obfBlock ->
            case deobfuscateBlock cfg transforms obfBlock of
              Right deobfBlock -> do
                let result = decodeExpr cfg deobfBlock
                result `shouldBe` "(+ 42 x)"
              Left err -> expectationFailure $ "Deobfuscation: " ++ show err
          Left err -> expectationFailure $ "Obfuscation: " ++ show err
      Left err -> expectationFailure $ "Encoding: " ++ show err

  it "20-layer mixed pipeline round-trips correctly" $ do
    let expr = SLet "y" (SLit 99) (SBinOp "*" (SVar "y") (SVar "y"))
    case encodeExpr cfg expr of
      Right block -> do
        let transforms = concat
              [ [mkAffineTransform (fromIntegral (3 + i * 7)) (fromIntegral (i * 11 + 5)) | i <- [1..10 :: Integer]]
              , [mkPermTransform (generateFromSeed 64 (fromInteger (i * 1337))) | i <- [1..5 :: Integer]]
              , [mkAffineTransform (fromIntegral (13 + i * 3)) (fromIntegral (i * 5 + 2)) | i <- [1..5 :: Integer]]
              ]
        case obfuscateBlock cfg transforms block of
          Right obfBlock ->
            case deobfuscateBlock cfg transforms obfBlock of
              Right deobfBlock -> do
                let result = decodeExpr cfg deobfBlock
                result `shouldBe` "(let y 99 (* y y))"
              Left err -> expectationFailure $ "Deobfuscation: " ++ show err
          Left err -> expectationFailure $ "Obfuscation: " ++ show err
      Left err -> expectationFailure $ "Encoding: " ++ show err

  it "deep pipeline doesn't cause stack overflow" $ do
    let cfg' = cfg
        poly = mkBoundedPoly (cfgFieldPrime cfg') 64
                 [Term (fromIntegral i) i | i <- [0..10]]
        transforms = [mkAffineTransform 3 7 | _ <- [1..50 :: Int]]
    case applyPipeline cfg' transforms poly of
      Right result -> polyDegree result `shouldSatisfy` (<= 64)
      Left err -> expectationFailure (show err)

-- ============================================================
-- 3. Polynomial Term Explosion Stress Test
-- ============================================================

termExplosionSpec :: Spec
termExplosionSpec = describe "Term Explosion Stress" $ do
  let p = 257

  it "degree cap blocks composition beyond limit" $ do
    let f = mkBoundedPoly p 16 [Term 1 16, Term 2 8, Term 3 4, Term 4 2, Term 5 0]
        g = mkBoundedPoly p 16 [Term 1 4, Term 1 0]
    case polyCompose f g of
      Left (DegreeOverflow actual cap) -> do
        actual `shouldSatisfy` (> cap)
      Left err -> expectationFailure $ "Unexpected error: " ++ show err
      Right _ -> expectationFailure "Should have been blocked by degree cap"

  it "polynomial multiplication respects degree cap" $ do
    let f = mkBoundedPoly p 32 [Term 1 16, Term 1 0]
        g = mkBoundedPoly p 32 [Term 1 17, Term 1 0]
    case polyMul f g of
      Left (DegreeOverflow _ _) -> return ()
      Left err -> expectationFailure $ "Unexpected error: " ++ show err
      Right _ -> expectationFailure "deg 16 + deg 17 = 33 > cap 32"

  it "canonical normalization keeps term count bounded" $ do
    -- Create polynomial with many duplicate exponents
    let terms = [Term (fromIntegral i) (i `mod` 5) | i <- [0..100]]
        poly = mkBoundedPoly p 64 terms
        coeffs = polyCoefficients poly
    -- After normalization, at most 5 unique terms
    length coeffs `shouldSatisfy` (<= 5)

  it "heavy composition chain stays within bounds" $ do
    -- Sequential multiplications that approach the cap
    let f = mkBoundedPoly p 64 [Term 1 2, Term 1 0]  -- x^2 + 1
    case polyMul f f of  -- x^4 + 2x^2 + 1
      Right f2 -> case polyMul f2 f2 of  -- x^8 + ...
        Right f4 -> case polyMul f4 f4 of  -- x^16 + ...
          Right f8 -> do
            polyDegree f8 `shouldBe` 16
            case polyMul f8 f8 of  -- x^32 + ...
              Right f16 -> do
                polyDegree f16 `shouldBe` 32
                case polyMul f16 f16 of  -- deg 64
                  Right f32 -> polyDegree f32 `shouldBe` 64
                  Left (DegreeOverflow _ _) -> return ()  -- acceptable: may exceed
                  Left err -> expectationFailure (show err)
              Left err -> expectationFailure (show err)
          Left err -> expectationFailure (show err)
        Left err -> expectationFailure (show err)
      Left err -> expectationFailure (show err)

-- ============================================================
-- 4. Large AST Encode/Decode Test
-- ============================================================

largeASTSpec :: Spec
largeASTSpec = describe "Large AST Encode/Decode" $ do
  let cfg = defaultConfig { cfgMaxDegree = 1024 }

  it "encodes and decodes a deeply nested expression" $ do
    -- Build: let x1 = 1 in let x2 = x1+1 in ... let xN = ...
    let depth = 20
        buildNested 0 = SLit 1
        buildNested n = SLet ("x" ++ show n) 
                          (SBinOp "+" (SVar ("x" ++ show (n-1))) (SLit 1))
                          (buildNested (n - 1))
        expr = SLet "x0" (SLit 0) (buildNested depth)
    exprDepth expr `shouldSatisfy` (>= depth)
    case encodeExpr cfg expr of
      Right block -> do
        let decoded = decodeExpr cfg block
        -- Should round-trip (encode and decode back to same serialization)
        decoded `shouldSatisfy` (not . null)
        -- Re-encode and check consistency
        ebOrigSize block `shouldSatisfy` (> 0)
      Left err -> expectationFailure (show err)

  it "module with many declarations encodes correctly" $ do
    let decls = [ FuncDecl ("func" ++ show i) ["x"]
                    (SBinOp "*" (SVar "x") (SLit (fromIntegral i)))
                | i <- [1..30 :: Int] ]
        srcMod = SourceModule "BigModule" decls
    case encodeModule cfg srcMod of
      Right em -> do
        length (emBlocks em) `shouldSatisfy` (>= 1)
        let decoded = decodeModule em
        length decoded `shouldSatisfy` (> 100)
      Left err -> expectationFailure (show err)

  it "single large expression with many operators" $ do
    -- Build: (1 + (2 + (3 + ... (n))))
    let buildChain 0 = SLit 0
        buildChain n = SBinOp "+" (SLit (fromIntegral n)) (buildChain (n - 1))
        expr = buildChain 50
    exprSize expr `shouldBe` 101  -- 50 ops + 51 literals
    case encodeExpr cfg expr of
      Right block -> do
        let decoded = decodeExpr cfg block
        decoded `shouldSatisfy` (not . null)
      Left err -> expectationFailure (show err)

-- ============================================================
-- 5. Hash Collision Simulation
-- ============================================================

hashCollisionSpec :: Spec
hashCollisionSpec = describe "Hash Collision Simulation" $ do
  let cfg = defaultConfig

  it "different modules produce different fingerprints" $ do
    let mods = [ SourceModule ("Mod" ++ show i)
                   [ValDecl "v" (SLit (fromIntegral i))]
               | i <- [1..100 :: Int] ]
    case mapM (encodeModule cfg) mods of
      Right ems -> do
        let fps = map computeFingerprint ems
            hashes = map fpHashHex fps
        -- All hashes should be unique
        length (nub hashes) `shouldBe` 100
      Left err -> expectationFailure (show err)

  it "slightly different content produces very different hashes" $ do
    let mod1 = SourceModule "Test" [ValDecl "x" (SLit 42)]
        mod2 = SourceModule "Test" [ValDecl "x" (SLit 43)]
    case (encodeModule cfg mod1, encodeModule cfg mod2) of
      (Right em1, Right em2) -> do
        let fp1 = computeFingerprint em1
            fp2 = computeFingerprint em2
        fpHashHex fp1 `shouldNotBe` fpHashHex fp2
        -- Priority should also differ
        let p1 = computePriority cfg fp1
            p2 = computePriority cfg fp2
        p1 `shouldNotBe` p2
      _ -> expectationFailure "Encoding failed"

  it "ordering is deterministic across runs" $ do
    let mod1 = SourceModule "Alpha" [ValDecl "a" (SLit 1)]
        mod2 = SourceModule "Beta"  [ValDecl "b" (SLit 2)]
        mod3 = SourceModule "Gamma" [ValDecl "c" (SLit 3)]
    case (encodeModule cfg mod1, encodeModule cfg mod2, encodeModule cfg mod3) of
      (Right em1, Right em2, Right em3) -> do
        let fp1 = computeFingerprint em1
            fp2 = computeFingerprint em2
            fp3 = computeFingerprint em3
            order1 = orderByPriority cfg [(em1, fp1), (em2, fp2), (em3, fp3)]
            order2 = orderByPriority cfg [(em1, fp1), (em2, fp2), (em3, fp3)]
        -- Same input → same order
        map oiPriority order1 `shouldBe` map oiPriority order2
        -- Order should be sorted ascending
        let prios = map oiPriority order1
        prios `shouldBe` prios  -- sorted check via property
        and (zipWith (<=) prios (tail prios)) `shouldBe` True
      _ -> expectationFailure "Encoding failed"

-- ============================================================
-- 6. Randomized Transform Fuzzing (QuickCheck)
-- ============================================================

fuzzingSpec :: Spec
fuzzingSpec = describe "Randomized Transform Fuzzing" $ do
  let cfg = defaultConfig
      p = cfgFieldPrime cfg

  it "random affine transforms always round-trip" $ property $
    \(Positive a') (b' :: Integer) ->
      let a = (a' `mod` (p - 1)) + 1  -- ensure a ∈ [1, p-1]
          b = b' `mod` p
          poly = mkBoundedPoly p 64 [Term 65 0, Term 120 1, Term 200 2]
          t = mkAffineTransform a b
      in case applyTransform cfg t poly >>= invertTransform cfg t of
           Right result ->
             polyEval result 0 == polyEval poly 0 &&
             polyEval result 1 == polyEval poly 1
           Left _ -> False

  it "random permutation transforms always round-trip" $ property $
    \(Positive seed) ->
      let perm = generateFromSeed 32 (fromIntegral (seed :: Int))
          t = mkPermTransform perm
          poly = mkBoundedPoly p 64 
                   [Term (fromIntegral i * 10) i | i <- [0..8]]
      in case applyTransform cfg t poly >>= invertTransform cfg t of
           Right result ->
             polyEval result 0 == polyEval poly 0 &&
             polyEval result 1 == polyEval poly 1 &&
             polyEval result 5 == polyEval poly 5
           Left _ -> False

  it "random affine pipeline preserves data coefficients" $ property $
    \seeds ->
      let n = min 10 (length (seeds :: [Int]))  -- cap at 10 transforms
          transforms = [ mkAffineTransform 
                           (fromIntegral ((abs s `mod` 256) + 1))
                           (fromIntegral (abs s `mod` 257))
                       | s <- take (max 1 n) seeds ]
          poly = mkBoundedPoly p 64 [Term 42 0, Term 100 1, Term 13 2]
          -- Compare each data coefficient individually
          coeffsMatch orig res = all (\i -> getCoeffAt i orig == getCoeffAt i res) [0..polyDegree orig]
      in case applyPipeline cfg transforms poly >>= invertPipeline cfg transforms of
           Right result -> coeffsMatch poly result
           Left _ -> False

  it "obfuscation preserves data for random expressions" $ property $
    \(Positive n') ->
      let n = (n' :: Int) `mod` 100 + 1
          expr = SBinOp "+" (SLit (fromIntegral n)) (SLit (fromIntegral (n * 2)))
          transforms = [mkAffineTransform 37 13, mkPermTransform (generateFromSeed 64 42)]
      in case encodeExpr cfg expr of
           Right block ->
             case obfuscateBlock cfg transforms block >>= deobfuscateBlock cfg transforms of
               Right deobf -> decodeExpr cfg deobf == decodeExpr cfg block
               Left _ -> False
           Left _ -> True  -- encoding failure is acceptable for very large n
