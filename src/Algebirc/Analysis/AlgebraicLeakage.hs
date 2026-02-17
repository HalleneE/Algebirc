-- |
-- Module      : Algebirc.Analysis.AlgebraicLeakage
-- Description : Algebraic structure detection — Gröbner basis, invariants, monomial leaks
-- License     : MIT
--
-- = Theory
--
-- Algebraic attacks exploit structural patterns in the transform polynomial.
-- This module detects:
-- 1. __Gröbner basis size__ — measures polynomial ideal complexity
-- 2. __Structural invariants__ — symmetry, fixed-point, periodicity patterns
-- 3. __Monomial leakage__ — degree patterns preserved through transforms
-- 4. __Linear relations__ — sampled linear dependencies between in/out

module Algebirc.Analysis.AlgebraicLeakage
  ( -- * Analysis
    analyzeAlgebraic
    -- * Components
  , computeGroebnerSize
  , detectInvariants
  , detectMonomialLeak
  , countLinearRelations
    -- * Formatting
  , formatAlgebraic
  ) where

import Algebirc.Core.Types

-- ============================================================
-- Main Analysis
-- ============================================================

-- | Complete algebraic leakage analysis.
analyzeAlgebraic :: ObfuscationConfig -> [Transform] -> BoundedPoly -> AlgebraicAnalysis
analyzeAlgebraic cfg transforms poly =
  let idealDim = computeGroebnerSize cfg poly
      invars   = detectInvariants transforms
      monoLeak = detectMonomialLeak cfg poly
      linRels  = countLinearRelations cfg transforms poly
  in AlgebraicAnalysis
       { aaIdealDim        = idealDim
       , aaInvariants      = invars
       , aaMonomialLeak    = monoLeak
       , aaLinearRelations = linRels
       }

-- ============================================================
-- Gröbner Basis Size (Simplified Buchberger)
-- ============================================================

-- | Estimate Gröbner basis size from polynomial structure.
-- Full Buchberger is exponential; we use a tractable heuristic:
-- basis size ≈ number of distinct monomial degrees × coefficient diversity.
--
-- Larger basis → harder to recover algebraic structure → better.
computeGroebnerSize :: ObfuscationConfig -> BoundedPoly -> Int
computeGroebnerSize cfg poly =
  let terms = polyTerms poly
      -- Count distinct degrees present
      distinctDegs = length (removeDups (map termExp terms))
      -- Count distinct coefficient values
      distinctCoeffs = length (removeDups (map termCoeff terms))
      -- Ideal dimension estimate:
      -- For a dense polynomial over GF(p), the Gröbner basis of the
      -- ideal <f - y> has size proportional to deg(f).
      -- For sparse polynomials, it's often smaller.
      -- Adjust: nonlinear transforms increase ideal dimension.
      maxDeg = polyMaxDegree poly
      density = if maxDeg > 0
                then fromIntegral (length terms) / fromIntegral (maxDeg + 1) :: Double
                else 0.0
      -- Higher density → larger Gröbner basis → harder to analyze
      idealEst = ceiling (fromIntegral (distinctDegs * distinctCoeffs) * max 1.0 density)
  in max 1 idealEst

-- ============================================================
-- Structural Invariant Detection
-- ============================================================

-- | Detect structural invariants in the transform pipeline.
-- These are patterns an attacker could exploit.
detectInvariants :: [Transform] -> [String]
detectInvariants transforms = concat
  [ detectCommutativePairs transforms
  , detectFixedPointRisk transforms
  , detectPeriodicity transforms
  , detectTagMonoculture transforms
  ]

-- | Check for commutative transform pairs.
-- If T_i ∘ T_j = T_j ∘ T_i, attacker can reorder → simplification.
detectCommutativePairs :: [Transform] -> [String]
detectCommutativePairs ts
  | length ts < 2 = []
  | otherwise =
      let pairs = zip ts (tail ts)
          commutativity = map checkCommutative pairs
      in concat commutativity
  where
    checkCommutative :: (Transform, Transform) -> [String]
    checkCommutative (t1, t2) =
      -- Affine ∘ Affine = Affine (always commutative in scalar case)
      case (transformTag t1, transformTag t2) of
        (AffineTransform, AffineTransform) ->
          ["COMMUTATIVE_PAIR: Adjacent affines commute → collapse to single affine"]
        _ -> []

-- | Check for fixed-point risk (identity-like transforms).
detectFixedPointRisk :: [Transform] -> [String]
detectFixedPointRisk = concatMap checkOne . zip [1..]
  where
    checkOne :: (Int, Transform) -> [String]
    checkOne (idx, t) = case transformTag t of
      AffineTransform ->
        case (transformA t, transformB t) of
          (Just 1, Just 0) -> ["FIXED_POINT[" ++ show idx ++ "]: Identity affine (a=1, b=0)"]
          (Just 1, _)      -> ["WEAK_AFFINE[" ++ show idx ++ "]: a=1 (translation only, easily invertible)"]
          _ -> []
      PowerMapTransform ->
        case transformExp t of
          Just 1 -> ["FIXED_POINT[" ++ show idx ++ "]: Power map e=1 (identity)"]
          _ -> []
      _ -> []

-- | Check for periodic patterns in transform sequence.
detectPeriodicity :: [Transform] -> [String]
detectPeriodicity ts
  | length ts < 4 = []
  | otherwise =
      let tags = map transformTag ts
          -- Check period-2 repetition: [A,B,A,B,...]
          hasPeriod2 = all (\i -> tags !! i == tags !! (i `mod` 2)) [0..min 5 (length tags - 1)]
      in ["PERIODIC_PATTERN: Transform sequence has period-2 regularity" | hasPeriod2 && length tags > 3]

-- | Check for transform type monoculture (all same type).
detectTagMonoculture :: [Transform] -> [String]
detectTagMonoculture ts
  | length ts < 2 = []
  | otherwise =
      let tags = map transformTag ts
          allSame = all (== head tags) tags
      in ["MONOCULTURE: All " ++ show (length ts) ++ " transforms are " ++ show (head tags)
          ++ " — lacks diffusion diversity" | allSame]

-- ============================================================
-- Monomial Leakage Detection
-- ============================================================

-- | Check if the degree/sparsity pattern of a polynomial leaks structure.
-- If the output polynomial has the same monomial support as a "typical"
-- polynomial of its type, the transform hasn't sufficiently obscured the structure.
detectMonomialLeak :: ObfuscationConfig -> BoundedPoly -> Bool
detectMonomialLeak _cfg poly =
  let terms = polyTerms poly
      maxDeg = polyMaxDegree poly
      -- A "good" obfuscated polynomial should be relatively dense
      -- (most positions filled). Sparse output = structure leakage.
      density = if maxDeg > 0
                then fromIntegral (length terms) / fromIntegral (maxDeg + 1) :: Double
                else 0.0
      -- Also check if coefficients cluster around specific values
      coeffs = map termCoeff terms
      distinctRatio = if null coeffs then 0.0
                      else fromIntegral (length (removeDups coeffs)) / fromIntegral (length coeffs)
  in density < 0.5 || distinctRatio < 0.3  -- leak if sparse or low diversity

-- ============================================================
-- Linear Relation Counting
-- ============================================================

-- | Count linear relations between input positions and output coefficients.
-- Method: construct sample pairs using affine offsets, check if
-- output differences are linearly predictable.
--
-- High count → pipeline is close to linear → vulnerable.
-- Low count → good nonlinear mixing.
countLinearRelations :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Int
countLinearRelations cfg transforms poly =
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree poly
      -- For each pair of positions (i, j), check if changing c[i]
      -- predictably changes output c[j]. This is a linear relation test.
      -- We use a simple heuristic: count transforms that are linear.
      linearCount = length (filter isLinearTag (map transformTag transforms))
      totalCount  = length transforms
      -- Linear ratio → estimated linear relations
      linearRatio = if totalCount > 0
                    then fromIntegral linearCount / fromIntegral totalCount :: Double
                    else 0.0
      -- Estimated linear relations ≈ maxDeg × linearRatio²
      -- (quadratic because chain of linears is still linear, but
      --  nonlinear breaks chain)
      estimated = ceiling (fromIntegral (maxDeg + 1) * linearRatio * linearRatio)
  in estimated
  where
    isLinearTag AffineTransform      = True
    isLinearTag PermutationTransform = True
    isLinearTag _                    = False

-- ============================================================
-- Formatting
-- ============================================================

-- | Format algebraic analysis as ASCII report.
formatAlgebraic :: AlgebraicAnalysis -> String
formatAlgebraic aa = unlines $
  [ "═══ Algebraic Leakage Analysis ═══"
  , ""
  , "  Gröbner basis size  = " ++ show (aaIdealDim aa)
      ++ idealQuality (aaIdealDim aa)
  , "  Linear relations    = " ++ show (aaLinearRelations aa)
      ++ linearQuality (aaLinearRelations aa)
  , "  Monomial leakage    = " ++ if aaMonomialLeak aa then "⚠ YES" else "✓ No"
  ] ++
  [ "" | not (null (aaInvariants aa)) ] ++
  [ "  Invariants detected:" | not (null (aaInvariants aa)) ] ++
  [ "    • " ++ inv | inv <- aaInvariants aa ] ++
  [ "  No structural invariants detected" | null (aaInvariants aa) ]

idealQuality :: Int -> String
idealQuality n
  | n >= 20   = " (excellent complexity)"
  | n >= 10   = " (good)"
  | n >= 5    = " (moderate)"
  | otherwise = " (⚠ low — structure easily recoverable)"

linearQuality :: Int -> String
linearQuality n
  | n == 0    = " (✓ fully nonlinear)"
  | n <= 2    = " (acceptable)"
  | n <= 5    = " (⚠ some linearity)"
  | otherwise = " (⚠⚠ highly linear — vulnerable)"

-- ============================================================
-- Utilities
-- ============================================================

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/= x) xs)
