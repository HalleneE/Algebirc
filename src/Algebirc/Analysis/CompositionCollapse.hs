-- |
-- Module      : Algebirc.Analysis.CompositionCollapse
-- Description : Scale-dependent collapse detection — entropy, Jacobian, degree growth
-- License     : MIT
--
-- = Theory
--
-- Pipeline yang aman pada 4 layer bisa collapse pada 16 layer.
-- Module ini test behavior pada skala berbeda dan deteksi:
-- 1. Entropy collapse (distribusi menyempit)
-- 2. Jacobian rank drop (algebraic dependency)
-- 3. Degree growth stagnation (nonlinearity hilang)
-- 4. Triple composition collapse (T_k ∘ T_j ∘ T_i = simpler)

module Algebirc.Analysis.CompositionCollapse
  ( -- * Test Utama
    testCompositionCollapse
  , testPairwiseCollapse
  , testTripleCollapse
    -- * Komponen
  , computeJacobianRank
  , measureDegreeGrowth
  , countFixedPoints
    -- * Formatting
  , formatCollapseResults
  ) where

import Algebirc.Core.Types
import Algebirc.Analysis.FormalEntropy (shannonEntropy)

-- ============================================================
-- Composition Collapse Test (Multi-Scale)
-- ============================================================

-- | Test composition collapse pada berbagai skala.
-- Untuk setiap layer count: apply pipeline, ukur entropy, Jacobian, degree growth.
testCompositionCollapse :: ObfuscationConfig
                        -> (BoundedPoly -> Either AlgebircError BoundedPoly)  -- ^ Single-layer pipeline
                        -> BoundedPoly                                        -- ^ Input test
                        -> [Int]                                              -- ^ Layer counts to test
                        -> [CollapseResult]
testCompositionCollapse cfg singleLayer inputPoly scales =
  map (testAtScale cfg singleLayer inputPoly) scales

-- | Test pada satu skala tertentu.
testAtScale :: ObfuscationConfig
            -> (BoundedPoly -> Either AlgebircError BoundedPoly)
            -> BoundedPoly
            -> Int
            -> CollapseResult
testAtScale cfg singleLayer inputPoly nLayers =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      n = maxDeg + 1

      -- Apply singleLayer N kali (composition)
      outputPoly = applyNTimes singleLayer nLayers inputPoly

      -- Entropy before/after
      inputCoeffs = [ getCoeffAt i inputPoly | i <- [0..maxDeg] ]
      outputCoeffs = [ getCoeffAt i outputPoly | i <- [0..maxDeg] ]
      hBefore = shannonEntropy inputCoeffs
      hAfter  = shannonEntropy outputCoeffs
      hDelta  = hAfter - hBefore

      -- Diversity: berapa persen koefisien unik
      distinctOut = length (removeDups outputCoeffs)
      diversity = fromIntegral distinctOut / fromIntegral n

      -- Fixed points: berapa posisi yang koefisiennya sama spt input
      fixedPts = countFixedPoints inputPoly outputPoly maxDeg

      -- Jacobian rank (numerik)
      jacRank = computeJacobianRank cfg singleLayer inputPoly nLayers
      expectedRank = n

      -- Degree growth
      degGrowth = measureDegreeGrowth cfg singleLayer inputPoly nLayers

      -- Collapse?
      collapsed = hDelta < -0.15         -- entropy drop > 15% dari base (tightened)
                || jacRank < n * 7 `div` 10  -- Jacobian rank < 70%
                || degGrowth < 0.70      -- degree < 70% expected
                || fixedPts > n `div` 3  -- >1/3 posisi fixed

  in CollapseResult
       { crLayers        = nLayers
       , crEntropyBefore = hBefore
       , crEntropyAfter  = hAfter
       , crEntropyDelta  = hDelta
       , crDiversity     = diversity
       , crFixedPoints   = fixedPts
       , crJacobianRank  = jacRank
       , crExpectedRank  = expectedRank
       , crDegreeGrowth  = degGrowth
       , crCollapsed     = collapsed
       }

-- ============================================================
-- Jacobian Rank (Numerik)
-- ============================================================

-- | Hitung Jacobian rank secara numerik.
-- J[i][j] = (f_i(x + e_j) - f_i(x)) mod p
-- dimana e_j = unit vector di posisi j.
computeJacobianRank :: ObfuscationConfig
                    -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                    -> BoundedPoly
                    -> Int  -- ^ layers
                    -> Int
computeJacobianRank cfg singleLayer basePoly nLayers =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      n = maxDeg + 1

      -- f(x) = N-kali composition
      pipeline = applyNTimes singleLayer nLayers
      baseOut = [ getCoeffAt i (pipeline basePoly) | i <- [0..maxDeg] ]

      -- Untuk setiap j: perturb posisi j += 1
      jacobianCols = [ perturbCol cfg singleLayer basePoly baseOut nLayers j p maxDeg
                      | j <- [0..maxDeg] ]

      -- Jacobian matrix: rows = output positions, cols = input positions
      jacobian = [ [ (jacobianCols !! j) !! i | j <- [0..n-1] ] | i <- [0..n-1] ]

  in gaussRank p jacobian

-- | Hitung satu kolom Jacobian: ∂f/∂x_j
perturbCol :: ObfuscationConfig
           -> (BoundedPoly -> Either AlgebircError BoundedPoly)
           -> BoundedPoly -> [Integer] -> Int -> Int -> Integer -> Int
           -> [Integer]
perturbCol cfg singleLayer basePoly baseOut nLayers j p maxDeg =
  let origCoeff = getCoeffAt j basePoly
      newCoeff  = (origCoeff + 1) `mod` p
      pertTerms = [ Term (if i == j then newCoeff else getCoeffAt i basePoly) i
                  | i <- [0..maxDeg] ]
      pertPoly = mkBoundedPoly p maxDeg pertTerms
      pertOutput = applyNTimes singleLayer nLayers pertPoly
      pertOut = [ getCoeffAt i pertOutput | i <- [0..maxDeg] ]
  in [ (b - a + p) `mod` p | (a, b) <- zip baseOut pertOut ]

-- ============================================================
-- Degree Growth
-- ============================================================

-- | Ukur degree growth setelah N layer.
-- Returns ratio actual_degree / expected_degree.
measureDegreeGrowth :: ObfuscationConfig
                    -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                    -> BoundedPoly -> Int -> Double
measureDegreeGrowth cfg singleLayer inputPoly nLayers =
  let outputPoly = applyNTimes singleLayer nLayers inputPoly
      actualDeg = polyDegree outputPoly
      maxDeg = cfgMaxDegree cfg
      -- Expected: degree dibatasi oleh maxDeg dari config
      expectedDeg = maxDeg
  in if expectedDeg > 0
     then fromIntegral actualDeg / fromIntegral expectedDeg
     else 0.0

-- ============================================================
-- Pairwise Collapse
-- ============================================================

-- | Test setiap pasangan (T_i, T_j) dalam pipeline.
-- Deteksi: apakah komposisi dua transform bisa disederhanakan.
testPairwiseCollapse :: ObfuscationConfig -> [Transform] -> BoundedPoly -> [(Int, Int, String)]
testPairwiseCollapse cfg transforms poly =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      n = length transforms
      pairs = [ (i, j) | i <- [0..n-2], j <- [i+1..n-1] ]
  in concatMap (checkPairCollapse cfg transforms poly p maxDeg) pairs

checkPairCollapse :: ObfuscationConfig -> [Transform] -> BoundedPoly
                  -> Integer -> Int -> (Int, Int) -> [(Int, Int, String)]
checkPairCollapse cfg transforms poly p maxDeg (i, j) =
  let ti = transforms !! i
      tj = transforms !! j
      -- Cek apakah keduanya affine (selalu collapse)
      bothAffine = transformTag ti == AffineTransform && transformTag tj == AffineTransform
      -- Cek apakah satu adalah identity-like
      isIdentityLike t = case transformTag t of
        AffineTransform -> transformA t == Just 1 && transformB t == Just 0
        PowerMapTransform -> transformExp t == Just 1
        _ -> False
  in [ (i, j, "AFFINE_COLLAPSE: transforms " ++ show i ++ " and " ++ show j ++ " collapse to single affine")
     | bothAffine ]
  ++ [ (i, j, "IDENTITY: transform " ++ show k ++ " is identity (wasted computation)")
     | (k, t) <- [(i, ti), (j, tj)], isIdentityLike t ]

-- ============================================================
-- Triple Collapse
-- ============================================================

-- | Test triple composition: (T_i, T_j, T_k).
-- Kadang collapse tidak muncul di pair, tapi muncul di triplet.
testTripleCollapse :: ObfuscationConfig -> [Transform] -> BoundedPoly -> [(Int, Int, Int, String)]
testTripleCollapse cfg transforms poly =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      n = length transforms
      triples = [ (i, j, k) | i <- [0..n-3], j <- [i+1..n-2], k <- [j+1..n-1] ]
  in concatMap (checkTriple cfg transforms poly p maxDeg) triples

checkTriple :: ObfuscationConfig -> [Transform] -> BoundedPoly
            -> Integer -> Int -> (Int, Int, Int) -> [(Int, Int, Int, String)]
checkTriple cfg transforms poly p maxDeg (i, j, k) =
  let ti = transforms !! i
      tj = transforms !! j
      tk = transforms !! k
      -- Cek: semua 3 affine → collapse ke 1 affine
      allAffine = all (\t -> transformTag t == AffineTransform) [ti, tj, tk]
      -- Cek: semua 3 tipe sama → monoculture risk
      allSameType = transformTag ti == transformTag tj && transformTag tj == transformTag tk
  in [ (i, j, k, "TRIPLE_AFFINE_COLLAPSE: transforms " ++ show i ++ "," ++ show j ++ "," ++ show k ++ " all affine")
     | allAffine ]
  ++ [ (i, j, k, "TRIPLE_MONOCULTURE: same type " ++ show (transformTag ti) ++ " at " ++ show i ++ "," ++ show j ++ "," ++ show k)
     | allSameType && not allAffine ]

-- ============================================================
-- Fixed Points
-- ============================================================

-- | Hitung fixed points: posisi dimana input coeffisien = output coeffisien.
countFixedPoints :: BoundedPoly -> BoundedPoly -> Int -> Int
countFixedPoints inputPoly outputPoly maxDeg =
  length [ () | i <- [0..maxDeg]
              , getCoeffAt i inputPoly == getCoeffAt i outputPoly ]

-- ============================================================
-- Utilities
-- ============================================================

-- | Apply function N kali.
applyNTimes :: (BoundedPoly -> Either AlgebircError BoundedPoly) -> Int -> BoundedPoly -> BoundedPoly
applyNTimes _ 0 poly = poly
applyNTimes f n poly = case f poly of
  Right next -> applyNTimes f (n - 1) next
  Left _     -> poly  -- stop on error

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/= x) xs)

-- | Gaussian elimination rank over GF(p).
gaussRank :: Integer -> [[Integer]] -> Int
gaussRank _ [] = 0
gaussRank p matrix =
  let rows = length matrix
      cols = if null matrix then 0 else length (head matrix)
  in go 0 0 matrix rows cols
  where
    go pr pc m rs cs
      | pr >= rs || pc >= cs = pr
      | otherwise =
          case [ r | r <- [pr..rs-1], (m !! r) !! pc `mod` p /= 0 ] of
            [] -> go pr (pc + 1) m rs cs
            (pivRow:_) ->
              let m1 = swapR pr pivRow m
                  pv = (m1 !! pr) !! pc
                  pvInv = modInv pv p
                  m2 = [ if r == pr then map (\x -> (x * pvInv) `mod` p) (m1 !! r) else m1 !! r
                       | r <- [0..length m1 - 1] ]
                  m3 = [ if r > pr && (m2 !! r) !! pc /= 0
                         then let f = (m2 !! r) !! pc
                              in zipWith (\a b -> (a - f * b + p * p) `mod` p) (m2 !! r) (m2 !! pr)
                         else m2 !! r
                       | r <- [0..rs-1] ]
              in go (pr + 1) (pc + 1) m3 rs cs

    swapR i j m
      | i == j = m
      | otherwise = [ if r == i then m !! j else if r == j then m !! i else m !! r
                     | r <- [0..length m - 1] ]

    modInv a m =
      let (g, x, _) = extGCD a m
      in if g == 1 then (x `mod` m + m) `mod` m else 1
    extGCD 0 b = (b, 0, 1)
    extGCD a' b' = let (g', x', y') = extGCD (b' `mod` a') a'
                   in (g', y' - (b' `div` a') * x', x')

-- ============================================================
-- Formatting
-- ============================================================

formatCollapseResults :: [CollapseResult] -> String
formatCollapseResults crs = unlines $
  [ "═══ Composition Collapse Analysis ═══", "" ] ++
  concatMap formatOne crs

formatOne :: CollapseResult -> [String]
formatOne cr =
  [ "  --- " ++ show (crLayers cr) ++ " layers ---"
  , "  Entropy:  " ++ showF (crEntropyBefore cr) ++ " → " ++ showF (crEntropyAfter cr)
      ++ " (Δ=" ++ showF (crEntropyDelta cr) ++ ")"
  , "  Diversity: " ++ showPct (crDiversity cr)
  , "  Fixed pts: " ++ show (crFixedPoints cr)
  , "  Jacobian:  " ++ show (crJacobianRank cr) ++ "/" ++ show (crExpectedRank cr)
      ++ jacobianGrade (crJacobianRank cr) (crExpectedRank cr)
  , "  Degree:    " ++ showPct (crDegreeGrowth cr)
  , "  Status:    " ++ if crCollapsed cr then "☠ COLLAPSED" else "✓ OK"
  , ""
  ]

jacobianGrade :: Int -> Int -> String
jacobianGrade actual expected
  | actual >= expected    = " ✓ (full rank)"
  | actual * 10 >= expected * 7 = " (acceptable)"
  | otherwise             = " ⚠ (DEPENDENCY DETECTED)"

showF :: Double -> String
showF d = show (fromIntegral (round (d * 100) :: Int) / 100.0 :: Double)

showPct :: Double -> String
showPct d = show (round (d * 100) :: Int) ++ "%"
