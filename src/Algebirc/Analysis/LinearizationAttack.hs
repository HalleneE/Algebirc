-- |
-- Module      : Algebirc.Analysis.LinearizationAttack
-- Description : Model linearization attacks and known-plaintext complexity
-- License     : MIT
--
-- = Theory
--
-- A linearization attack treats the obfuscation pipeline as a black-box
-- function f: GF(p)^n → GF(p)^n and attempts to recover f from
-- input-output pairs.
--
-- For a linear function: d+1 pairs suffice (Gaussian elimination).
-- For nonlinear functions: the number of required pairs grows exponentially
-- with the nonlinear depth.
--
-- This module:
-- 1. Builds a linearization matrix from the pipeline structure
-- 2. Computes its rank via Gaussian elimination over GF(p)
-- 3. Estimates the number of known pairs needed for full recovery

module Algebirc.Analysis.LinearizationAttack
  ( -- * Analysis
    analyzeAttack
    -- * Components
  , buildLinearizationMatrix
  , matrixRank
  , knownPairsNeeded
  , attackComplexity
    -- * Formatting
  , formatAttack
  ) where

import Algebirc.Core.Types
import qualified Data.Map.Strict as Map

-- ============================================================
-- Main Analysis
-- ============================================================

-- | Complete linearization attack analysis.
analyzeAttack :: ObfuscationConfig -> [Transform] -> BoundedPoly -> AttackAnalysis
analyzeAttack cfg transforms poly =
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree poly
      matrix = buildLinearizationMatrix cfg transforms poly
      rank   = matrixRank p matrix
      pairs  = knownPairsNeeded transforms
      compl  = attackComplexity transforms
      vuln   = isVulnerable transforms
      rec    = recommendation transforms
  in AttackAnalysis
       { laMatrixRank     = rank
       , laKnownPairs     = pairs
       , laComplexity     = compl
       , laVulnerable     = vuln
       , laRecommendation = rec
       }

-- ============================================================
-- Linearization Matrix Construction
-- ============================================================

-- | Build linearization matrix from pipeline structure.
-- Each row represents the "linear component" of a transform's action
-- on the coefficient vector.
--
-- For affine transforms: exact matrix representation
-- For nonlinear: linear approximation (first-order Taylor)
buildLinearizationMatrix :: ObfuscationConfig -> [Transform] -> BoundedPoly -> [[Integer]]
buildLinearizationMatrix cfg transforms _poly =
  let p = cfgFieldPrime cfg
      n = cfgMaxDegree cfg + 1
      -- Start with identity matrix
      identity = [ [ if i == j then 1 else 0 | j <- [0..n-1] ] | i <- [0..n-1] ]
      -- Apply each transform's linear component
  in foldl (composeMatrix p n) identity transforms

-- | Compose a transform's linear component with the current matrix.
composeMatrix :: Integer -> Int -> [[Integer]] -> Transform -> [[Integer]]
composeMatrix p n current t = case transformTag t of
  -- Affine: multiply each row by 'a' (mod p)
  AffineTransform ->
    case transformA t of
      Just a  -> [ [ (a * x) `mod` p | x <- row ] | row <- current ]
      Nothing -> current

  -- Permutation: reorder columns
  PermutationTransform ->
    case transformPerm t of
      Just perm ->
        let mapping = permMapping perm
            permCol row = [ row !! findPerm mapping j | j <- [0..n-1] ]
        in map permCol current
      Nothing -> current

  -- S-Box: nonlinear → zero linear component (treated as random)
  SBoxTransform -> zeroMatrix n

  -- Feistel: nonlinear → near-zero linear component
  FeistelTransform -> zeroMatrix n

  -- Power map: nonlinear → zero linear component for e > 1
  PowerMapTransform ->
    case transformExp t of
      Just 1  -> current  -- identity
      _       -> zeroMatrix n

  -- Polynomial: depends on degree
  PolynomialTransform ->
    case transformPoly t of
      Just poly ->
        if polyDegree poly <= 1
        then current  -- degree-1 is linear
        else zeroMatrix n  -- nonlinear
      Nothing -> current

  -- Composite: compose sub-transforms
  CompositeTransform ->
    foldl (composeMatrix p n) current (transformSubs t)

  -- ARX diffusion: nonlinear → zero linear component
  ARXDiffusionTransform -> zeroMatrix n

-- | Zero matrix (represents total nonlinearity — no linear component).
zeroMatrix :: Int -> [[Integer]]
zeroMatrix n = [ [ 0 | _ <- [1..n] ] | _ <- [1..n] ]

-- | Find permutation target for index j.
findPerm :: Map.Map Int Int -> Int -> Int
findPerm mapping j = case Map.lookup j mapping of
  Just target -> target
  Nothing     -> j  -- identity fallback

-- ============================================================
-- Gaussian Elimination over GF(p)
-- ============================================================

-- | Compute matrix rank via Gaussian elimination over GF(p).
matrixRank :: Integer -> [[Integer]] -> Int
matrixRank p matrix =
  let rows = length matrix
  in if rows == 0 then 0
     else let cols = length (head matrix)
          in gaussianRank p rows cols matrix

gaussianRank :: Integer -> Int -> Int -> [[Integer]] -> Int
gaussianRank p rows cols mat =
  go 0 0 mat
  where
    go pivotRow pivotCol m
      | pivotRow >= rows || pivotCol >= cols = pivotRow
      | otherwise =
          -- Find non-zero entry in current column from pivotRow down
          case findPivot pivotRow pivotCol m of
            Nothing -> go pivotRow (pivotCol + 1) m  -- skip column
            Just pr ->
              let -- Swap rows
                  m1 = swapRows pivotRow pr m
                  -- Get pivot value
                  pivotVal = (m1 !! pivotRow) !! pivotCol
                  -- Scale pivot row so pivot = 1
                  pivotInv = modInverse pivotVal p
                  m2 = mapRow pivotRow (\x -> (x * pivotInv) `mod` p) m1
                  -- Eliminate below
                  m3 = eliminateBelow pivotRow pivotCol m2
              in go (pivotRow + 1) (pivotCol + 1) m3

    findPivot pr pc m =
      let candidates = [ (r, (m !! r) !! pc) | r <- [pr..rows-1] ]
          nonZero = filter (\(_, v) -> v `mod` p /= 0) candidates
      in case nonZero of
           ((r, _):_) -> Just r
           []         -> Nothing

    swapRows i j m
      | i == j = m
      | otherwise =
          [ if r == i then m !! j
            else if r == j then m !! i
            else m !! r
          | r <- [0..length m - 1] ]

    mapRow i f m =
      [ if r == i then map f (m !! r) else m !! r
      | r <- [0..length m - 1] ]

    eliminateBelow pr pc m =
      [ if r > pr && (m !! r) !! pc /= 0
        then let factor = (m !! r) !! pc
             in zipWith (\a b -> (a - factor * b + p * p) `mod` p)
                        (m !! r) (m !! pr)
        else m !! r
      | r <- [0..length m - 1] ]

-- | Modular inverse via extended GCD.
modInverse :: Integer -> Integer -> Integer
modInverse a m =
  let (g, x, _) = extGCD a m
  in if g == 1 then (x `mod` m + m) `mod` m else 1
  where
    extGCD 0 b = (b, 0, 1)
    extGCD a' b' =
      let (g', x', y') = extGCD (b' `mod` a') a'
      in (g', y' - (b' `div` a') * x', x')

-- ============================================================
-- Attack Complexity Estimation
-- ============================================================

-- | Estimate minimum known-plaintext pairs needed for full recovery.
knownPairsNeeded :: [Transform] -> Int
knownPairsNeeded transforms =
  let tags = map transformTag transforms
      -- Linear transforms: d+1 pairs suffice
      -- Each nonlinear layer multiplies the requirement
      linearLayers = length (filter isLinear tags)
      nonlinearLayers = length (filter (not . isLinear) tags)
      basePairs = linearLayers + 1  -- minimum for linear parts
      -- Each nonlinear layer exponentially increases requirement
      nonlinearMultiplier = 2 ^ min nonlinearLayers 20
  in basePairs * nonlinearMultiplier

-- | Describe attack complexity class.
attackComplexity :: [Transform] -> String
attackComplexity transforms =
  let tags = map transformTag transforms
      nonlinearCount = length (filter (not . isLinear) tags)
      totalCount = length tags
  in if nonlinearCount == 0
     then "O(d) — trivial (all-linear pipeline)"
     else if nonlinearCount == 1
     then "O(p) — single nonlinear layer (brute-forceable for small p)"
     else if nonlinearCount <= 3
     then "O(p^" ++ show nonlinearCount ++ ") — moderate (multi-layer nonlinear)"
     else "O(2^" ++ show (nonlinearCount * 8) ++ ") — computationally infeasible"

-- | Is the pipeline vulnerable to linearization?
isVulnerable :: [Transform] -> Bool
isVulnerable transforms =
  let tags = map transformTag transforms
      nonlinearCount = length (filter (not . isLinear) tags)
  in nonlinearCount < 2  -- vulnerable if fewer than 2 nonlinear layers

-- | Generate fix recommendation.
recommendation :: [Transform] -> String
recommendation transforms
  | isVulnerable transforms =
      "Add nonlinear layers (S-Box, Feistel, PowerMap) to resist linearization"
  | otherwise =
      "Pipeline has sufficient nonlinear depth — no immediate action needed"

-- | Is a transform tag linear?
isLinear :: TransformTag -> Bool
isLinear AffineTransform      = True
isLinear PermutationTransform = True
isLinear _                    = False

-- ============================================================
-- Formatting
-- ============================================================

-- | Format attack analysis as ASCII report.
formatAttack :: AttackAnalysis -> String
formatAttack aa = unlines
  [ "═══ Linearization Attack Analysis ═══"
  , ""
  , "  Matrix rank        = " ++ show (laMatrixRank aa)
  , "  Known pairs needed = " ++ show (laKnownPairs aa)
  , "  Attack complexity  = " ++ laComplexity aa
  , ""
  , "  Vulnerability: " ++ if laVulnerable aa then "⚠ VULNERABLE" else "✓ RESISTANT"
  , "  Recommendation: " ++ laRecommendation aa
  ]
