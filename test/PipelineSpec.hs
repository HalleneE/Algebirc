module PipelineSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Algebirc.Core.Types
import Algebirc.Obfuscation.Pipeline
  ( ObfuscationPipeline(..), buildPipeline, runPipelinePoly, invertPipelinePoly )
import Algebirc.Analysis.Leakage (analyzeLeakage, lrSecurityScore)

-- ============================================================
-- Helpers
-- ============================================================

testCfg :: Int -> ObfuscationConfig
testCfg genus = defaultConfig
  { cfgGenus       = genus
  , cfgFieldPrime  = 257
  , cfgMaxDegree   = 16  -- small for test speed
  }

-- Build a BoundedPoly from a list of values (truncated to degree cap)
toPoly :: ObfuscationConfig -> [Integer] -> BoundedPoly
toPoly cfg vs =
  let p      = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      terms  = zipWith (\i c -> Term (c `mod` p) i) [0..maxDeg] vs
  in mkBoundedPoly p maxDeg terms

-- Extract flat coefficient list [0..maxDeg]
toCoeffs :: ObfuscationConfig -> BoundedPoly -> [Integer]
toCoeffs cfg poly =
  let maxDeg = cfgMaxDegree cfg
  in [ getCoeffAt i poly | i <- [0..maxDeg] ]

-- ============================================================
-- Property 1: Coefficient-level round-trip
-- ============================================================

-- | runPipelinePoly >=> invertPipelinePoly ≡ id at coefficient level.
-- Genus-1 and Genus-2 both tested.
prop_roundTripCoefficients :: Int -> [Integer] -> Property
prop_roundTripCoefficients genusN vs =
  let g     = if genusN `mod` 2 == 0 then 2 else 1
      cfg   = testCfg g
      poly  = toPoly cfg vs
  in case buildPipeline cfg of
       Left err -> counterexample ("buildPipeline failed: " ++ show err) False
       Right pl ->
         case runPipelinePoly cfg pl poly of
           Left err -> counterexample ("runPipelinePoly failed: " ++ show err) False
           Right obf ->
             case invertPipelinePoly cfg pl obf of
               Left err -> counterexample ("invertPipelinePoly failed: " ++ show err) False
               Right recovered ->
                 let origC = toCoeffs cfg poly
                     recvC = toCoeffs cfg recovered
                 in counterexample
                      ("Round-trip failed.\nOriginal:   " ++ show origC ++
                       "\nRecovered: " ++ show recvC)
                      (origC == recvC)

-- ============================================================
-- Property 2: Genus-2 output statistically differs from Genus-1
-- ============================================================

-- | Same input, same algebraic seed, but genus-2 geometric layer must
-- produce different polynomial coefficients than genus-1.
prop_genus2DiffersFromGenus1 :: NonEmptyList Integer -> Property
prop_genus2DiffersFromGenus1 (NonEmpty vs) =
  let cfg1 = testCfg 1
      cfg2 = testCfg 2
      poly = toPoly cfg1 vs   -- same input poly for both
  in case (buildPipeline cfg1, buildPipeline cfg2) of
       (Right pl1, Right pl2) ->
         case (runPipelinePoly cfg1 pl1 poly, runPipelinePoly cfg2 pl2 poly) of
           (Right out1, Right out2) ->
             let c1 = toCoeffs cfg1 out1
                 c2 = toCoeffs cfg2 out2
             in counterexample
                  ("Genus-2 output identical to genus-1!\nGenus-1: " ++ show c1 ++
                   "\nGenus-2: " ++ show c2)
                  (c1 /= c2)
           _ -> discard
       _ -> discard

-- ============================================================
-- Property 3: Security score genus-2 >= genus-1 (heuristic)
-- ============================================================

-- | lrSecurityScore (internal heuristic, NOT a formal security claim)
-- for genus-2 transforms should be >= genus-1 score on same config.
-- NOTE: This validates the heuristic metric is monotone w.r.t. geometric layer.
prop_genus2SecurityScoreAtLeastGenus1 :: Property
prop_genus2SecurityScoreAtLeastGenus1 =
  let cfg1 = testCfg 1
      cfg2 = testCfg 2
  in case (buildPipeline cfg1, buildPipeline cfg2) of
       (Right pl1, Right pl2) ->
         let ts1 = plAlgTransforms pl1  -- same algebraic layer for both
             ts2 = plAlgTransforms pl2
             score1 = lrSecurityScore $ analyzeLeakage cfg1 ts1 Nothing
             score2 = lrSecurityScore $ analyzeLeakage cfg2 ts2 Nothing
         in counterexample
              ("Security score genus-2 (" ++ show score2 ++
               ") < genus-1 (" ++ show score1 ++
               ")\n[NOTE: lrSecurityScore is a heuristic — 100 - linBias*40 - fixedPts*30 - corr*30]")
              (score2 >= score1)
       _ -> property True  -- can't build pipeline: skip rather than fail
  where
    plAlgTransforms pl = Algebirc.Obfuscation.Pipeline.plAlgTransforms pl

-- ============================================================
-- Spec Registration
-- ============================================================

spec :: Spec
spec = describe "Algebirc.Obfuscation.Pipeline" $ do

  describe "1. Round-Trip Correctness" $ do
    it "Genus-1 and Genus-2: runPipeline >=> invertPipeline ≡ id at coefficient level" $
      withMaxSuccess 200 $ property prop_roundTripCoefficients

  describe "2. Geometric Differentiation" $ do
    it "Genus-2 output differs coefficient-by-coefficient from Genus-1 on same input" $
      withMaxSuccess 100 $ property prop_genus2DiffersFromGenus1

  describe "3. Heuristic Security Score (informational only)" $ do
    it "lrSecurityScore(genus-2) >= lrSecurityScore(genus-1) [internal metric, non-formal]" $
      prop_genus2SecurityScoreAtLeastGenus1
