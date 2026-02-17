-- |
-- Module      : IntegrationSpec
-- Description : Tests untuk Phase 4 real-world integration
--
-- Tests:
-- 1. Parse real .hs → SourceModule
-- 2. Source → Haskell generation
-- 3. Obfuscate → deobfuscate round-trip
-- 4. CLI pipeline test

module IntegrationSpec (spec) where

import Test.Hspec
import Algebirc.Core.Types
import Algebirc.Obfuscation.AST
import Algebirc.Obfuscation.Encoder
import Algebirc.Obfuscation.Transform
import Algebirc.Integration.HaskellParser
import Algebirc.Integration.FileIO
import System.Directory (removeFile, doesFileExist)

spec :: Spec
spec = do
  parserSpec
  codeGenSpec
  pipelineSpec

-- ============================================================
-- 1. Haskell Parser Tests
-- ============================================================

parserSpec :: Spec
parserSpec = describe "HaskellParser" $ do

  it "parse simple function definition" $ do
    let src = "module Test where\nfoo x = x + 1"
    case parseHaskellSource src of
      Right pr -> do
        smName (prModule pr) `shouldBe` "Test"
        length (smDecls (prModule pr)) `shouldSatisfy` (>= 1)
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "parse module with multiple declarations" $ do
    let src = unlines
          [ "module Multi where"
          , "foo x = x + 1"
          , "bar y = y * 2"
          , "baz = 42"
          ]
    case parseHaskellSource src of
      Right pr -> do
        smName (prModule pr) `shouldBe` "Multi"
        length (smDecls (prModule pr)) `shouldSatisfy` (>= 3)
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "parse lambda expressions" $ do
    let src = "module Lam where\nf = \\x -> x + 1"
    case parseHaskellSource src of
      Right pr -> do
        smName (prModule pr) `shouldBe` "Lam"
        length (smDecls (prModule pr)) `shouldSatisfy` (>= 1)
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "parse if-then-else" $ do
    let src = "module Cond where\nabs' x = if x > 0 then x else negate x"
    case parseHaskellSource src of
      Right pr -> do
        length (smDecls (prModule pr)) `shouldSatisfy` (>= 1)
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "parse let expression" $ do
    let src = "module LetTest where\nf x = let y = x + 1 in y * 2"
    case parseHaskellSource src of
      Right pr ->
        length (smDecls (prModule pr)) `shouldSatisfy` (>= 1)
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "parse list and tuple" $ do
    let src = "module ListTest where\nxs = [1, 2, 3]\npair = (1, 2)"
    case parseHaskellSource src of
      Right pr ->
        length (smDecls (prModule pr)) `shouldSatisfy` (>= 2)
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "returns error for invalid syntax" $ do
    let src = "module Bad where\nfoo = if then else"
    case parseHaskellSource src of
      Right _ -> expectationFailure "Expected parse error"
      Left _  -> return ()

  it "handles module without explicit module header" $ do
    let src = "foo x = x + 1"
    case parseHaskellSource src of
      Right pr -> smName (prModule pr) `shouldBe` "Main"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- ============================================================
-- 2. Code Generation Tests
-- ============================================================

codeGenSpec :: Spec
codeGenSpec = describe "Code Generation" $ do

  it "generates valid Haskell from SourceModule" $ do
    let sm = SourceModule "Test"
               [ FuncDecl "foo" ["x"] (SBinOp "+" (SVar "x") (SLit 1))
               , ValDecl "bar" (SLit 42)
               ]
        output = sourceToHaskell sm
    output `shouldContain` "module Test where"
    output `shouldContain` "foo x ="
    output `shouldContain` "bar ="

  it "round-trip: parse → generate preserves module name" $ do
    let src = "module RoundTrip where\nfoo x = x + 1"
    case parseHaskellSource src of
      Right pr -> do
        let generated = sourceToHaskell (prModule pr)
        generated `shouldContain` "module RoundTrip where"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- ============================================================
-- 3. Pipeline Tests
-- ============================================================

pipelineSpec :: Spec
pipelineSpec = describe "Obfuscation Pipeline" $ do
  let cfg = defaultConfig

  it "encode → obfuscate → deobfuscate → decode round-trip" $ do
    let src = "module RT where\nadd a b = a + b"
    case parseHaskellSource src of
      Right pr -> do
        let sm = prModule pr
            transforms = defaultPipeline cfg
            expr = case head (smDecls sm) of
                     FuncDecl _ _ body -> body
                     ValDecl _ body -> body
        case encodeExpr cfg expr of
          Left err -> expectationFailure $ "Encode error: " ++ show err
          Right block ->
            case obfuscateBlock cfg transforms block of
              Left err -> expectationFailure $ "Obfuscate error: " ++ show err
              Right obfBlock ->
                case deobfuscateBlock cfg transforms obfBlock of
                  Left err -> expectationFailure $ "Deobfuscate error: " ++ show err
                  Right deobfBlock -> do
                    let decoded = decodeExpr cfg deobfBlock
                        original = decodeExpr cfg block
                    decoded `shouldBe` original
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "obfuscateSource works on parsed module" $ do
    let src = "module Obf where\nsquare x = x * x"
    case parseHaskellSource src of
      Right pr ->
        case obfuscateSource cfg src (prModule pr) of
          Left err -> expectationFailure $ "Obfuscation error: " ++ show err
          Right od -> do
            length (odBlocks od) `shouldSatisfy` (>= 1)
            smName (odModule od) `shouldBe` "Obf"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "file obfuscation round-trip" $ do
    let src = "module FileRT where\nfoo x = x + 1"
        inputPath = "/tmp/algebirc_test_input.hs"
        outputPath = "/tmp/algebirc_test_output.hs"
        metaPath = outputPath ++ ".meta"
        recoveredPath = "/tmp/algebirc_test_recovered.hs"

    -- Tulis input
    writeFile inputPath src

    -- Obfuscate
    obfResult <- obfuscateFile cfg inputPath outputPath
    case obfResult of
      Left err -> expectationFailure $ "Obfuscate error: " ++ err
      Right _ -> do
        -- Cek file output ada
        exists <- doesFileExist outputPath
        exists `shouldBe` True

        -- Deobfuscate
        deobfResult <- deobfuscateFile cfg metaPath recoveredPath
        case deobfResult of
          Left err -> expectationFailure $ "Deobfuscate error: " ++ err
          Right recovered -> do
            -- Source asli harus sama
            recovered `shouldContain` "module FileRT where"
            recovered `shouldContain` "foo x = x + 1"

        -- Cleanup
        mapM_ (\f -> doesFileExist f >>= \e -> if e then removeFile f else return ())
              [inputPath, outputPath, metaPath, recoveredPath]

  it "parse real sample file" $ do
    let src = unlines
          [ "module Sample where"
          , ""
          , "factorial :: Int -> Int"
          , "factorial 0 = 1"
          , "factorial n = n * factorial (n - 1)"
          , ""
          , "square :: Int -> Int"
          , "square x = x * x"
          ]
    case parseHaskellSource src of
      Right pr -> do
        smName (prModule pr) `shouldBe` "Sample"
        -- Harus ada factorial dan square (plus type sigs)
        length (smDecls (prModule pr)) `shouldSatisfy` (>= 4)
      Left err -> expectationFailure $ "Parse failed: " ++ err
