-- |
-- Module      : Algebirc.Analysis.StructuralProbe
-- Description : Algebraic shortcut, low-degree annihilator, invariant propagation
-- License     : MIT
--
-- = Theory
--
-- Pipeline yang "terlihat" aman bisa punya shortcut algebra:
-- 1. Effective degree jauh lebih rendah dari expected → shortcut!
-- 2. Ada low-degree annihilator g(x) st g(f(x)) ≡ 0 → fatal!
-- 3. Structural invariant (sum, parity, symmetry) survive pipeline → leak!

module Algebirc.Analysis.StructuralProbe
  ( -- * Probe Utama
    probeAlgebraicShortcut
  , probeInvariantPropagation
    -- * Komponen
  , measureEffectiveDegree
  , detectLowDegreeAnnihilator
  , probeSymmetry
  , probeAdditiveInvariant
  , probeParityInvariant
  , probeTranslationInvariant
    -- * Formatting
  , formatProbeResult
  , formatInvariants
  ) where

import Algebirc.Core.Types
import Algebirc.Analysis.FormalEntropy (shannonEntropy)

-- ============================================================
-- Algebraic Shortcut Detection
-- ============================================================

-- | Probe utama: apakah pipeline punya algebraic shortcut?
-- Cek effective degree, annihilator, faktorisabilitas output.
probeAlgebraicShortcut :: ObfuscationConfig
                       -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                       -> BoundedPoly
                       -> ProbeResult
probeAlgebraicShortcut cfg pipeline inputPoly =
  let maxDeg = cfgMaxDegree cfg
      p = cfgFieldPrime cfg

      -- Effective degree: degree aktual output polynomial
      effectiveDeg = measureEffectiveDegree cfg pipeline inputPoly
      expectedDeg  = maxDeg
      degRatio = if expectedDeg > 0
                 then fromIntegral effectiveDeg / fromIntegral expectedDeg
                 else 0.0

      -- Low-degree annihilator test
      annihDeg = detectLowDegreeAnnihilator cfg pipeline inputPoly

      -- Faktorisasi sederhana: cek output punya common factor
      factorCount = detectSimpleFactors cfg pipeline inputPoly

      -- Shortcut ditemukan kalau:
      -- 1. Degree < 70% expected, ATAU
      -- 2. Ada low-degree annihilator, ATAU
      -- 3. Output bisa difaktorkan menjadi >1 faktor sederhana
      shortcut = degRatio < 0.70
               || annihDeg > 0
               || factorCount > 1

  in ProbeResult
       { prEffectiveDegree = effectiveDeg
       , prExpectedDegree  = expectedDeg
       , prDegreeRatio     = degRatio
       , prShortcutFound   = shortcut
       , prAnnihilatorDeg  = annihDeg
       , prFactorCount     = factorCount
       }

-- ============================================================
-- Effective Degree
-- ============================================================

-- | Ukur effective degree: degree aktual output polynomial.
-- Kalau jauh lebih rendah dari maxDeg → degree collapse!
measureEffectiveDegree :: ObfuscationConfig
                       -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                       -> BoundedPoly
                       -> Int
measureEffectiveDegree cfg pipeline inputPoly =
  case pipeline inputPoly of
    Right outPoly -> polyDegree outPoly
    Left _        -> 0

-- ============================================================
-- Low-Degree Annihilator Detection
-- ============================================================

-- | Cari apakah ada polynomial g(x) degree ≤ 8 sehingga g(f(x)) ≡ 0 mod p.
-- Kalau ada → attacker bisa reduce pipeline algebra!
--
-- Method: untuk setiap candidate degree d (1..8):
--   1. Sample output values dari pipeline
--   2. Build Vandermonde matrix dari output values
--   3. Cek apakah ada non-trivial solution a₀ + a₁y + ... + a_d y^d ≡ 0
--      untuk semua sample y
--   4. Jika rank < d+1 → annihilator ada di degree d!
detectLowDegreeAnnihilator :: ObfuscationConfig
                           -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                           -> BoundedPoly
                           -> Int  -- ^ Degree annihilator terkecil (0 = none)
detectLowDegreeAnnihilator cfg pipeline inputPoly =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg

      -- Sample beberapa input, kumpulkan output values
      sampleInputs = [ mkBoundedPoly p maxDeg [Term (fromIntegral k) 0] | k <- [0..min 20 maxDeg] ]
                  ++ [ mkBoundedPoly p maxDeg
                         [ Term ((fromIntegral k * fromIntegral i * 7 + 13) `mod` p) i
                         | i <- [0..maxDeg] ]
                     | k <- [1..10 :: Int] ]
      outputValues = concatMap (\inp -> case pipeline inp of
        Right out -> [ getCoeffAt i out | i <- [0.. min (polyMaxDegree out) maxDeg] ]
        Left _    -> []) sampleInputs

      -- Deduplicate and take enough samples
      samples = take 30 (removeDups outputValues)

      -- Test each degree from 1 to 8
      annihDeg = findAnnihilatorDeg p samples 1

  in annihDeg

-- | Find smallest degree d ∈ [1..8] where a degree-d polynomial
-- annihilates all sample values. Returns 0 if none found.
findAnnihilatorDeg :: Integer -> [Integer] -> Int -> Int
findAnnihilatorDeg _ _ d | d > 8 = 0  -- not found up to deg 8
findAnnihilatorDeg p samples d =
  let -- Need at least d+1 samples to test degree d
      needed = d + 1
      testSamples = take (needed + 5) samples  -- extra for confidence

      -- Build Vandermonde matrix: V[i][j] = sample_i^j mod p
      -- If rank(V) < d+1, then there exists a non-trivial annihilator
      vandermonde = [ [ (y ^ fromIntegral j) `mod` p | j <- [0..d] ]
                    | y <- testSamples ]

      rank = gaussRankP p vandermonde

      -- Annihilator exists if rank < d+1 (non-trivial kernel)
      hasAnnihilator = length testSamples >= needed && rank < d + 1

  in if hasAnnihilator then d
     else findAnnihilatorDeg p samples (d + 1)

-- | Gaussian elimination rank over GF(p) for annihilator detection.
gaussRankP :: Integer -> [[Integer]] -> Int
gaussRankP _ [] = 0
gaussRankP p matrix =
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
                  pvInv = modInvP pv p
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

    modInvP a m =
      let (g, x, _) = extGCD a m
      in if g == 1 then (x `mod` m + m) `mod` m else 1
    extGCD 0 b = (b, 0, 1)
    extGCD a' b' = let (g', x', y') = extGCD (b' `mod` a') a'
                   in (g', y' - (b' `div` a') * x', x')

-- ============================================================
-- Simple Factorization
-- ============================================================

-- | Deteksi apakah output polynomial punya common factor.
-- Kalau semua koefisien punya GCD > 1 → trivial factor!
detectSimpleFactors :: ObfuscationConfig
                    -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                    -> BoundedPoly
                    -> Int
detectSimpleFactors cfg pipeline inputPoly =
  case pipeline inputPoly of
    Right outPoly ->
      let coeffs = [ getCoeffAt i outPoly | i <- [0..polyMaxDegree outPoly] ]
          nonZero = filter (/= 0) coeffs
          commonGCD = if null nonZero then 0
                      else foldl1 gcd nonZero
          -- Kalau GCD > 1, artinya p | semua koefisien → trivial factor
          -- Hitung "faktor" sebagai jumlah pembagi kecil
          factorCount = if commonGCD > 1 && commonGCD < cfgFieldPrime cfg
                        then countSmallFactors commonGCD
                        else 0
      in factorCount
    Left _ -> 0

countSmallFactors :: Integer -> Int
countSmallFactors n = length [ d | d <- [2..min 100 n], n `mod` d == 0 ]

-- ============================================================
-- Invariant Propagation
-- ============================================================

-- | Cek semua known invariant: mana yang survive pipeline?
probeInvariantPropagation :: ObfuscationConfig
                          -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                          -> BoundedPoly
                          -> [InvariantLeak]
probeInvariantPropagation cfg pipeline inputPoly =
  [ probeAdditiveInvariant cfg pipeline inputPoly
  , probeParityInvariant cfg pipeline inputPoly
  , probeTranslationInvariant cfg pipeline inputPoly
  ] ++ probeSymmetry cfg pipeline inputPoly

-- | Cek: apakah sum(coefficients) preserved setelah pipeline?
-- Kalau ya → additif invariant → CRITICAL!
probeAdditiveInvariant :: ObfuscationConfig
                       -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                       -> BoundedPoly
                       -> InvariantLeak
probeAdditiveInvariant cfg pipeline inputPoly =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      inputSum = sum [ getCoeffAt i inputPoly | i <- [0..maxDeg] ] `mod` p

      -- Test beberapa input berbeda
      testInputs = [ mkBoundedPoly p maxDeg
                       [ Term ((fromIntegral k * fromIntegral i + 7) `mod` p) i
                       | i <- [0..maxDeg] ]
                   | k <- [1..5 :: Int] ]
      results = map (\inp ->
        let inSum = sum [ getCoeffAt i inp | i <- [0..maxDeg] ] `mod` p
        in case pipeline inp of
             Right out ->
               let outSum = sum [ getCoeffAt i out | i <- [0..maxDeg] ] `mod` p
               in inSum == outSum
             Left _ -> False
        ) testInputs

      allPreserved = all id results && not (null results)

  in InvariantLeak
       { ilProperty = "SUM_PRESERVATION: sum(coefficients) mod p"
       , ilSurvives = allPreserved
       , ilSeverity = if allPreserved then "CRITICAL" else "OK"
       }

-- | Cek: apakah parity (even/odd sum) preserved?
probeParityInvariant :: ObfuscationConfig
                     -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                     -> BoundedPoly
                     -> InvariantLeak
probeParityInvariant cfg pipeline inputPoly =
  let maxDeg = cfgMaxDegree cfg
      p = cfgFieldPrime cfg

      testInputs = [ mkBoundedPoly p maxDeg
                       [ Term ((fromIntegral k * fromIntegral i + 13) `mod` p) i
                       | i <- [0..maxDeg] ]
                   | k <- [1..8 :: Int] ]
      results = map (\inp ->
        let inParity = sum [ getCoeffAt i inp | i <- [0..maxDeg] ] `mod` 2
        in case pipeline inp of
             Right out ->
               let outParity = sum [ getCoeffAt i out | i <- [0..maxDeg] ] `mod` 2
               in inParity == outParity
             Left _ -> False
        ) testInputs

      allPreserved = all id results && not (null results)

  in InvariantLeak
       { ilProperty = "PARITY_PRESERVATION: sum(coefficients) mod 2"
       , ilSurvives = allPreserved
       , ilSeverity = if allPreserved then "WARNING" else "OK"
       }

-- | Cek: apakah f(x+k) = f(x) + k? (translation invariance)
probeTranslationInvariant :: ObfuscationConfig
                          -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                          -> BoundedPoly
                          -> InvariantLeak
probeTranslationInvariant cfg pipeline inputPoly =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg

      -- f(x): output of base input
      baseOut = case pipeline inputPoly of
        Right out -> [ getCoeffAt i out | i <- [0..maxDeg] ]
        Left _    -> []

      -- f(x + k) vs f(x) + k, untuk k = 1, 2, 3
      translations = [ k | k <- [1, 2, 3 :: Integer] ]
      results = map (\k ->
        let -- x + k: tambah k ke setiap koefisien
            shiftedTerms = [ Term ((getCoeffAt i inputPoly + k) `mod` p) i | i <- [0..maxDeg] ]
            shiftedPoly = mkBoundedPoly p maxDeg shiftedTerms
        in case pipeline shiftedPoly of
             Right shiftedOut ->
               let outCoeffs = [ getCoeffAt i shiftedOut | i <- [0..maxDeg] ]
                   -- f(x) + k:
                   expectedOut = [ (c + k) `mod` p | c <- baseOut ]
               in outCoeffs == expectedOut
             Left _ -> False
        ) translations

      anyPreserved = any id results

  in InvariantLeak
       { ilProperty = "TRANSLATION_INVARIANCE: f(x+k) = f(x) + k"
       , ilSurvives = anyPreserved
       , ilSeverity = if anyPreserved then "CRITICAL" else "OK"
       }

-- | Cek symmetry: f(x) = f(-x)? f(x) = f(p-1-x)?
probeSymmetry :: ObfuscationConfig
              -> (BoundedPoly -> Either AlgebircError BoundedPoly)
              -> BoundedPoly
              -> [InvariantLeak]
probeSymmetry cfg pipeline inputPoly =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg

      -- f(x):
      baseOut = case pipeline inputPoly of
        Right out -> [ getCoeffAt i out | i <- [0..maxDeg] ]
        Left _    -> []

      -- f(-x) = f(p - x): negate koefisien
      negTerms = [ Term ((p - getCoeffAt i inputPoly) `mod` p) i | i <- [0..maxDeg] ]
      negPoly = mkBoundedPoly p maxDeg negTerms
      negOut = case pipeline negPoly of
        Right out -> [ getCoeffAt i out | i <- [0..maxDeg] ]
        Left _    -> []

      isEvenSymmetric = baseOut == negOut && not (null baseOut)

      -- f(p-1-x): complement
      compTerms = [ Term ((p - 1 - getCoeffAt i inputPoly) `mod` p) i | i <- [0..maxDeg] ]
      compPoly = mkBoundedPoly p maxDeg compTerms
      compOut = case pipeline compPoly of
        Right out -> [ getCoeffAt i out | i <- [0..maxDeg] ]
        Left _    -> []

      isCompSymmetric = baseOut == compOut && not (null baseOut)

  in [ InvariantLeak
         { ilProperty = "EVEN_SYMMETRY: f(x) = f(-x mod p)"
         , ilSurvives = isEvenSymmetric
         , ilSeverity = if isEvenSymmetric then "WARNING" else "OK"
         }
     , InvariantLeak
         { ilProperty = "COMPLEMENT_SYMMETRY: f(x) = f(p-1-x)"
         , ilSurvives = isCompSymmetric
         , ilSeverity = if isCompSymmetric then "WARNING" else "OK"
         }
     ]

-- ============================================================
-- Formatting
-- ============================================================

formatProbeResult :: ProbeResult -> String
formatProbeResult pr = unlines
  [ "═══ Structural Probe Result ═══"
  , ""
  , "  Effective degree: " ++ show (prEffectiveDegree pr) ++ "/" ++ show (prExpectedDegree pr)
      ++ " (" ++ showPct (prDegreeRatio pr) ++ ")"
      ++ degreeGrade (prDegreeRatio pr)
  , "  Annihilator deg: " ++ if prAnnihilatorDeg pr == 0
                             then "None (✓ safe)"
                             else show (prAnnihilatorDeg pr) ++ " ☠ FATAL!"
  , "  Factor count:    " ++ show (prFactorCount pr)
  , ""
  , "  Shortcut: " ++ if prShortcutFound pr then "⚠ FOUND!" else "✓ None detected"
  ]

degreeGrade :: Double -> String
degreeGrade r
  | r >= 0.9  = " ✓ (excellent)"
  | r >= 0.7  = " (acceptable)"
  | r >= 0.5  = " ⚠ (degree collapse)"
  | otherwise = " ☠ (critical shortcut)"

formatInvariants :: [InvariantLeak] -> String
formatInvariants leaks = unlines $
  [ "═══ Invariant Propagation ═══", "" ] ++
  map formatOneLeak leaks

formatOneLeak :: InvariantLeak -> String
formatOneLeak il =
  "  " ++ ilProperty il ++ ": "
  ++ if ilSurvives il then "⚠ SURVIVES [" ++ ilSeverity il ++ "]"
     else "✓ Destroyed"

showPct :: Double -> String
showPct d = show (round (d * 100) :: Int) ++ "%"

-- ============================================================
-- Utilities
-- ============================================================

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/= x) xs)
