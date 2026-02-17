-- |
-- Module      : Algebirc.Analysis.FormalEntropy
-- Description : Formal entropy analysis with min-entropy and adversarial bounds
-- License     : MIT
--
-- = Theory
--
-- * __Shannon__: H(X) = -Σ pᵢ log₂ pᵢ — average information content
-- * __Min-entropy__: H∞(X) = -log₂(max pᵢ) — worst-case guessing
-- * __Adversarial__: I(input; output | structure, ¬key) — mutual information
-- * __Invariant__: H∞ ≤ H_Shannon ≤ H_max = log₂(|support|)

module Algebirc.Analysis.FormalEntropy
  ( -- * Analysis
    analyzeEntropy
    -- * Components
  , shannonEntropy
  , minEntropy
  , maxEntropy
  , adversarialInfo
  , entropyGrade
    -- * Formatting
  , formatEntropy
  ) where

import Algebirc.Core.Types

-- ============================================================
-- Main Analysis
-- ============================================================

-- | Complete formal entropy analysis of a polynomial's coefficient distribution.
-- Measures Shannon, min-entropy, redundancy, adversarial information, and assigns grade.
analyzeEntropy :: ObfuscationConfig -> BoundedPoly -> EntropyAnalysis
analyzeEntropy cfg poly =
  let coeffs = allCoeffs (polyMaxDegree poly) poly
      hShannon = shannonEntropy coeffs
      hMin     = minEntropy coeffs
      hMax     = maxEntropy coeffs
      redund   = if hMax > 0 then 1.0 - (hShannon / hMax) else 1.0
      adv      = adversarialInfo cfg poly
      grade    = entropyGrade' hShannon hMin hMax
  in EntropyAnalysis
       { eaShannon     = hShannon
       , eaMinEntropy  = hMin
       , eaMaxEntropy  = hMax
       , eaRedundancy  = redund
       , eaAdversarial = adv
       , eaGrade       = grade
       }

-- | Extract all coefficient values (including zeros) for positions 0..maxDeg.
allCoeffs :: Int -> BoundedPoly -> [Integer]
allCoeffs maxDeg poly = [ getCoeffAt i poly | i <- [0..maxDeg] ]

-- ============================================================
-- Shannon Entropy
-- ============================================================

-- | Shannon entropy H(X) = -Σ pᵢ log₂ pᵢ
-- Where pᵢ = frequency of value i / total count.
shannonEntropy :: [Integer] -> Double
shannonEntropy [] = 0.0
shannonEntropy xs =
  let n = fromIntegral (length xs) :: Double
      freqs = countFrequencies xs
      probs = map (\c -> fromIntegral c / n) freqs
  in negate $ sum [ p * logBase 2 p | p <- probs, p > 0 ]

-- ============================================================
-- Min-Entropy
-- ============================================================

-- | Min-entropy H∞(X) = -log₂(max pᵢ)
-- Conservative bound: assumes adversary always guesses the most likely value.
minEntropy :: [Integer] -> Double
minEntropy [] = 0.0
minEntropy xs =
  let n = fromIntegral (length xs) :: Double
      freqs = countFrequencies xs
      maxFreq = maximum freqs
      pMax = fromIntegral maxFreq / n
  in if pMax > 0 then negate (logBase 2 pMax) else 0.0

-- ============================================================
-- Max Entropy
-- ============================================================

-- | Maximum possible entropy = log₂(|support|)
-- where |support| = number of distinct coefficient values.
maxEntropy :: [Integer] -> Double
maxEntropy [] = 0.0
maxEntropy xs =
  let distinct = length (removeDuplicates xs)
  in if distinct > 0 then logBase 2 (fromIntegral distinct) else 0.0

-- ============================================================
-- Adversarial Information
-- ============================================================

-- | Estimate adversarial mutual information I(input; output | structure).
-- Method: measure coefficient variance — low variance means high predictability.
--
-- We compute the coefficient of variation (CV = σ/μ) across all positions.
-- High CV → high entropy → low adversarial info.
-- Low CV → concentrated distribution → information leakage.
adversarialInfo :: ObfuscationConfig -> BoundedPoly -> Double
adversarialInfo cfg poly =
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree poly
      coeffs = [ fromIntegral (getCoeffAt i poly) :: Double | i <- [0..maxDeg] ]
      n = fromIntegral (length coeffs)
      mean = sum coeffs / n
      variance = sum [ (c - mean)^(2 :: Int) | c <- coeffs ] / n
      stddev = sqrt variance
      -- Ideal: uniform over GF(p), stddev ≈ p/√12
      idealStddev = fromIntegral p / sqrt 12.0
      -- Adversarial info: how far from ideal?
      -- 0 = perfect (uniform), 1 = deterministic
      ratio = if idealStddev > 0
              then max 0 (1.0 - stddev / idealStddev)
              else 1.0
  in ratio

-- ============================================================
-- Entropy Grading
-- ============================================================

-- | Assign entropy grade A-F based on min-entropy ratio.
--
-- A: H∞ ≥ 0.90 × H_max — near-uniform, excellent
-- B: H∞ ≥ 0.75 × H_max — good diffusion
-- C: H∞ ≥ 0.60 × H_max — acceptable
-- D: H∞ ≥ 0.40 × H_max — weak, needs improvement
-- F: H∞ < 0.40 × H_max — critical vulnerability
entropyGrade :: EntropyAnalysis -> Char
entropyGrade ea = entropyGrade' (eaShannon ea) (eaMinEntropy ea) (eaMaxEntropy ea)

entropyGrade' :: Double -> Double -> Double -> Char
entropyGrade' _ hMin hMax
  | hMax <= 0      = 'F'
  | ratio >= 0.90  = 'A'
  | ratio >= 0.75  = 'B'
  | ratio >= 0.60  = 'C'
  | ratio >= 0.40  = 'D'
  | otherwise      = 'F'
  where ratio = hMin / hMax

-- ============================================================
-- Formatting
-- ============================================================

-- | Format entropy analysis as ASCII report.
formatEntropy :: EntropyAnalysis -> String
formatEntropy ea = unlines
  [ "═══ Formal Entropy Analysis ═══"
  , ""
  , "  Shannon H(X)      = " ++ showF (eaShannon ea) ++ " bits"
  , "  Min-entropy H∞(X) = " ++ showF (eaMinEntropy ea) ++ " bits"
  , "  Max entropy H_max = " ++ showF (eaMaxEntropy ea) ++ " bits"
  , "  Redundancy        = " ++ showF (eaRedundancy ea * 100) ++ "%"
  , "  Adversarial I     = " ++ showF (eaAdversarial ea)
  , ""
  , "  Grade: " ++ [eaGrade ea] ++ gradeDesc (eaGrade ea)
  , ""
  , "  Invariant check: H∞ ≤ H ≤ H_max → "
      ++ if eaMinEntropy ea <= eaShannon ea + 0.001
            && eaShannon ea <= eaMaxEntropy ea + 0.001
         then "✓ SATISFIED"
         else "✗ VIOLATED"
  ]

gradeDesc :: Char -> String
gradeDesc 'A' = " — Near-uniform distribution, excellent diffusion"
gradeDesc 'B' = " — Good diffusion, minor bias"
gradeDesc 'C' = " — Acceptable, some structure visible"
gradeDesc 'D' = " — Weak entropy, improvement needed"
gradeDesc 'F' = " — Critical: highly predictable"
gradeDesc _   = " — Unknown"

showF :: Double -> String
showF d = let rounded = fromIntegral (round (d * 1000) :: Int) / 1000.0 :: Double
          in show rounded

-- ============================================================
-- Utilities
-- ============================================================

-- | Count value frequencies (sorted for stability).
countFrequencies :: [Integer] -> [Int]
countFrequencies [] = []
countFrequencies xs =
  let sorted = qsort xs
  in runs sorted

runs :: [Integer] -> [Int]
runs [] = []
runs (x:xs') = let (same, rest) = span (== x) xs'
               in (1 + length same) : runs rest

qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (x:xs') = qsort smaller ++ [x] ++ qsort larger
  where smaller = filter (< x) xs'
        larger  = filter (>= x) xs'

-- | Remove duplicates from a list.
removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [] = []
removeDuplicates (x:xs') = x : removeDuplicates (filter (/= x) xs')
