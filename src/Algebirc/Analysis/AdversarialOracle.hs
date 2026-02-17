-- |
-- Module      : Algebirc.Analysis.AdversarialOracle
-- Description : Known-plaintext, chosen-plaintext, dan differential attack simulation
-- License     : MIT
--
-- = Attacker Model
--
-- Attacker tahu: pipeline structure (public)
-- Attacker TIDAK tahu: secret key
-- Attacker bisa: kumpulkan input/output pairs
-- Attacker mau: recover original polynomial atau key

module Algebirc.Analysis.AdversarialOracle
  ( -- * Serangan Utama
    knownPlaintextAttack
  , chosenPlaintextAttack
  , differentialAttack
    -- * Input Generators
  , generateProbeInputs
    -- * Formatting
  , formatOracleResult
  , formatDiffResult
  ) where

import Algebirc.Core.Types
import Algebirc.Analysis.FormalEntropy (shannonEntropy)

-- ============================================================
-- Known-Plaintext Attack
-- ============================================================

-- | Simulasi known-plaintext attack.
-- Kumpulkan N pasangan (input, output), coba recover transform via
-- linear approximation. Ukur: rank drop, solution space, residual error.
knownPlaintextAttack :: ObfuscationConfig
                     -> (BoundedPoly -> Either AlgebircError BoundedPoly)  -- ^ Pipeline function
                     -> Int                                                -- ^ Jumlah pairs
                     -> OracleAttackResult
knownPlaintextAttack cfg pipeline nPairs =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      n = maxDeg + 1  -- jumlah posisi koefisien

      -- Generate probe inputs: unit vectors + structured
      probeInputs = take nPairs (generateProbeInputs cfg)

      -- Jalankan pipeline pada setiap input
      pairs = [ (inp, runPipeline pipeline inp) | inp <- probeInputs ]

      -- Build linearization matrix dari pairs
      -- Setiap pair memberikan n persamaan linear (satu per posisi output)
      matrix = buildSystemMatrix p n pairs

      -- Hitung rank via Gaussian elimination
      rank = gaussRank p matrix
      expectedRank = min (length pairs * n) (n * n)
      rankDrop = if expectedRank > 0
                 then 1.0 - fromIntegral rank / fromIntegral expectedRank
                 else 0.0

      -- Solution space: p^(n - rank)
      solSpace = p ^ max 0 (n - rank)

      -- Korelasi: rata-rata correlation antara input dan output posisi yang sama
      correlation = measureCorrelation p n pairs

      -- Residual error: seberapa jauh output dari linear approximation
      residual = measureResidual p n pairs

      -- Recovery berhasil kalau rank = full DAN residual ≈ 0
      recovered = rank >= n && residual < 0.01

      vuln | recovered  = "CRITICAL: Pipeline recovered via " ++ show nPairs ++ " pairs"
           | rank >= n `div` 2 = "WARNING: Partial recovery, rank " ++ show rank ++ "/" ++ show n
           | rankDrop > 0.3 = "INFO: Significant rank drop (" ++ show (round (rankDrop * 100) :: Int) ++ "%)"
           | otherwise = "OK: Attack failed, pipeline resistant"

  in OracleAttackResult
       { oaRecovered     = recovered
       , oaPairsUsed     = nPairs
       , oaRankDrop      = rankDrop
       , oaSolutionSpace = solSpace
       , oaCorrelation   = correlation
       , oaResidualError = residual
       , oaVulnerability = vuln
       }

-- ============================================================
-- Chosen-Plaintext Attack
-- ============================================================

-- | Simulasi chosen-plaintext attack dengan input terstruktur.
-- Input types: unit vectors, all-zeros, all-ones, low Hamming weight,
-- monomial basis, repeated patterns.
chosenPlaintextAttack :: ObfuscationConfig
                      -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                      -> OracleAttackResult
chosenPlaintextAttack cfg pipeline =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      n = maxDeg + 1

      -- Structured inputs (lebih berbahaya dari random)
      structuredInputs = generateStructuredInputs cfg

      -- Jalankan pipeline
      pairs = [ (inp, runPipeline pipeline inp) | inp <- structuredInputs ]

      -- Analisis korelasi per input class
      correlation = measureCorrelation p n pairs

      -- Build system dan ukur rank
      matrix = buildSystemMatrix p n pairs
      rank = gaussRank p matrix
      expectedRank = n

      rankDrop = if expectedRank > 0
                 then 1.0 - fromIntegral rank / fromIntegral expectedRank
                 else 0.0

      solSpace = p ^ max 0 (n - rank)
      residual = measureResidual p n pairs
      recovered = rank >= n && residual < 0.01

      vuln | recovered  = "CRITICAL: Chosen-plaintext recovery succeeded"
           | correlation > 0.7 = "WARNING: High correlation (" ++ show (round (correlation * 100) :: Int) ++ "%) — structure leaks"
           | rankDrop < 0.1 = "WARNING: Low rank drop — pipeline may be close to linear"
           | otherwise = "OK: Chosen-plaintext attack failed"

  in OracleAttackResult
       { oaRecovered     = recovered
       , oaPairsUsed     = length structuredInputs
       , oaRankDrop      = rankDrop
       , oaSolutionSpace = solSpace
       , oaCorrelation   = correlation
       , oaResidualError = residual
       , oaVulnerability = vuln
       }

-- ============================================================
-- Differential Attack
-- ============================================================

-- | Differential attack: ubah 1 koefisien, ukur semua delta output.
-- Target: avalanche ≥ 50% (pseudo-random diffusion).
differentialAttack :: ObfuscationConfig
                   -> (BoundedPoly -> Either AlgebircError BoundedPoly)
                   -> BoundedPoly
                   -> DiffResult
differentialAttack cfg pipeline basePoly =
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree basePoly
      n = maxDeg + 1

      baseOutput = runPipeline pipeline basePoly

      -- Untuk setiap posisi i: ubah c[i] += 1, ukur delta output
      pertResults = [ analyzeOnePert cfg pipeline basePoly baseOutput i p maxDeg | i <- [0..maxDeg] ]

      -- Hitung statistik
      deadCount   = length [ () | (isDead, _, _) <- pertResults, isDead ]
      linearCount = length [ () | (_, isLin, _) <- pertResults, isLin ]

      avalanches = [ ratio | (_, _, ratio) <- pertResults ]
      avgAvalanche = if null avalanches then 0.0
                     else sum avalanches / fromIntegral (length avalanches)

      -- Korelasi: cari posisi output yang paling terprediksi
      maxCorr = if null avalanches then 0.0 else maximum avalanches

      -- Detail per posisi
      details = [ "pos[" ++ show i ++ "]: avalanche=" ++ showPct ratio
                  ++ (if isDead then " DEAD" else "")
                  ++ (if isLin then " LINEAR" else "")
                | (i, (isDead, isLin, ratio)) <- zip [0..] pertResults ]

      hasDiff = deadCount > 0 || linearCount > n `div` 3

  in DiffResult
       { drDeadPositions   = deadCount
       , drLinearPositions = linearCount
       , drAvalancheRatio  = avgAvalanche
       , drMaxCorrelation  = maxCorr
       , drIsDifferential  = hasDiff
       , drDetails         = details
       }

-- | Analisis satu perturbation pada posisi i.
-- Returns: (isDead, isLinear, avalancheRatio)
analyzeOnePert :: ObfuscationConfig
               -> (BoundedPoly -> Either AlgebircError BoundedPoly)
               -> BoundedPoly  -> [Integer] -> Int -> Integer -> Int
               -> (Bool, Bool, Double)
analyzeOnePert cfg pipeline basePoly baseOut pos p maxDeg =
  let -- Perturb koefisien di posisi pos: c[pos] += 1
      origCoeff = getCoeffAt pos basePoly
      newCoeff = (origCoeff + 1) `mod` p
      pertTerms = [ Term (if i == pos then newCoeff else getCoeffAt i basePoly) i
                  | i <- [0..maxDeg] ]
      pertPoly = mkBoundedPoly p maxDeg pertTerms
      pertOutput = runPipeline pipeline pertPoly

      n = maxDeg + 1
      -- Hitung berapa posisi yang berubah
      changed = length [ () | (a, b) <- zip baseOut pertOutput, a /= b ]
      avalanche = fromIntegral changed / fromIntegral n

      -- Dead: tidak ada output yang berubah
      isDead = changed == 0

      -- Linear: semua delta konstan atau proporsional?
      deltas = [ (b - a + p) `mod` p | (a, b) <- zip baseOut pertOutput ]
      nonZeroDeltas = filter (/= 0) deltas
      isLinear' = not (null nonZeroDeltas) && all (== head nonZeroDeltas) nonZeroDeltas

  in (isDead, isLinear', avalanche)

-- ============================================================
-- Input Generators
-- ============================================================

-- | Generate probe inputs: unit vectors, structured patterns.
generateProbeInputs :: ObfuscationConfig -> [BoundedPoly]
generateProbeInputs cfg =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
  in generateStructuredInputs cfg ++ generateRandomishInputs cfg

-- | Structured inputs (attacker biasa pakai ini).
generateStructuredInputs :: ObfuscationConfig -> [BoundedPoly]
generateStructuredInputs cfg =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
  in concat
       [ -- Unit vectors: [0,...,1,...,0] di setiap posisi
         [ mkBoundedPoly p maxDeg [Term 1 i] | i <- [0..maxDeg] ]
         -- All-zeros
       , [ mkBoundedPoly p maxDeg [] ]
         -- All-ones
       , [ mkBoundedPoly p maxDeg [ Term 1 i | i <- [0..maxDeg] ] ]
         -- Repeated patterns: [k,k,...,k]
       , [ mkBoundedPoly p maxDeg [ Term k i | i <- [0..maxDeg] ] | k <- [2, p `div` 2, p - 1] ]
         -- Low Hamming weight: 2 koefisien non-zero
       , [ mkBoundedPoly p maxDeg [Term 1 i, Term 1 j]
         | i <- [0..min 3 maxDeg], j <- [i+1..min 4 maxDeg] ]
         -- Monomial basis: x^0, x^1, ..., x^d (masing-masing koefisien = p/2)
       , [ mkBoundedPoly p maxDeg [Term (p `div` 2) i] | i <- [0..maxDeg] ]
       ]

-- | Pseudo-random-ish inputs (via deterministic sequence).
generateRandomishInputs :: ObfuscationConfig -> [BoundedPoly]
generateRandomishInputs cfg =
  let p = cfgFieldPrime cfg
      maxDeg = cfgMaxDegree cfg
      seeds = [100, 200, 300, 400, 500]
  in [ mkBoundedPoly p maxDeg
         [ Term ((seed * fromIntegral i * 6364136223846793005 + 1442695040888963407) `mod` p) i
         | i <- [0..maxDeg] ]
     | seed <- seeds ]

-- ============================================================
-- Internal: Linear System Construction
-- ============================================================

-- | Jalankan pipeline, return koefisien output sebagai list.
runPipeline :: (BoundedPoly -> Either AlgebircError BoundedPoly) -> BoundedPoly -> [Integer]
runPipeline f poly = case f poly of
  Right out -> [ getCoeffAt i out | i <- [0.. polyMaxDegree out] ]
  Left _    -> [ getCoeffAt i poly | i <- [0.. polyMaxDegree poly] ]

-- | Build system matrix dari input/output pairs.
-- Setiap pair memberikan persamaan: output = M * input (linear model).
buildSystemMatrix :: Integer -> Int -> [(BoundedPoly, [Integer])] -> [[Integer]]
buildSystemMatrix p n pairs =
  let -- Flatten: setiap pair → n rows, setiap row = input coeffs
      rows = concatMap (\(inp, out) ->
        [ [ getCoeffAt j inp | j <- [0..n-1] ] | _ <- out ]
        ) pairs
  in take (n * 2) rows  -- cap to prevent huge matrices

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
          case findPivot pr pc m rs of
            Nothing -> go pr (pc + 1) m rs cs
            Just pivRow ->
              let m1 = swapR pr pivRow m
                  pv = (m1 !! pr) !! pc
                  pvInv = modInv pv p
                  m2 = modifyRow pr (\x -> (x * pvInv) `mod` p) m1
                  m3 = elimBelow pr pc m2 rs
              in go (pr + 1) (pc + 1) m3 rs cs

    findPivot pr pc m rs =
      case [ r | r <- [pr..rs-1], (m !! r) !! pc `mod` p /= 0 ] of
        (r:_) -> Just r
        []    -> Nothing

    swapR i j m
      | i == j = m
      | otherwise = [ if r == i then m !! j else if r == j then m !! i else m !! r
                     | r <- [0..length m - 1] ]

    modifyRow i f m = [ if r == i then map f (m !! r) else m !! r
                       | r <- [0..length m - 1] ]

    elimBelow pr pc m rs =
      [ if r > pr && (m !! r) !! pc /= 0
        then let f = (m !! r) !! pc
             in zipWith (\a b -> (a - f * b + p * p) `mod` p) (m !! r) (m !! pr)
        else m !! r
      | r <- [0..rs-1] ]

    modInv a m =
      let (g, x, _) = extGCD a m
      in if g == 1 then (x `mod` m + m) `mod` m else 1
    extGCD 0 b = (b, 0, 1)
    extGCD a' b' = let (g', x', y') = extGCD (b' `mod` a') a'
                   in (g', y' - (b' `div` a') * x', x')

-- | Ukur korelasi rata-rata antara input dan output.
measureCorrelation :: Integer -> Int -> [(BoundedPoly, [Integer])] -> Double
measureCorrelation p n pairs =
  let -- Untuk setiap pair, hitung berapa posisi yang "mirip"
      similarities = map (\(inp, out) ->
        let inCoeffs = [ getCoeffAt i inp | i <- [0..n-1] ]
            matchCount = length [ () | (a, b) <- zip inCoeffs out
                                     , a == b || (a + 1) `mod` p == b ]
        in fromIntegral matchCount / fromIntegral n
        ) pairs
  in if null similarities then 0.0 else sum similarities / fromIntegral (length similarities)

-- | Ukur residual error linear approximation.
measureResidual :: Integer -> Int -> [(BoundedPoly, [Integer])] -> Double
measureResidual _ n pairs =
  let -- Residual: variance of (output - input) per posisi
      diffs = concatMap (\(inp, out) ->
        [ abs (fromIntegral (getCoeffAt i inp) - fromIntegral b) :: Double
        | (i, b) <- zip [0..] out, i < n ]
        ) pairs
  in if null diffs then 0.0
     else let mean = sum diffs / fromIntegral (length diffs)
          in mean / fromIntegral (max 1 n)

-- ============================================================
-- Formatting
-- ============================================================

formatOracleResult :: OracleAttackResult -> String
formatOracleResult oa = unlines
  [ "═══ Oracle Attack Result ═══"
  , "  Recovered:      " ++ show (oaRecovered oa)
  , "  Pairs used:     " ++ show (oaPairsUsed oa)
  , "  Rank drop:      " ++ showPct (oaRankDrop oa)
  , "  Solution space: " ++ show (oaSolutionSpace oa)
  , "  Correlation:    " ++ showPct (oaCorrelation oa)
  , "  Residual error: " ++ show (oaResidualError oa)
  , "  → " ++ oaVulnerability oa
  ]

formatDiffResult :: DiffResult -> String
formatDiffResult dr = unlines $
  [ "═══ Differential Attack Result ═══"
  , "  Dead positions:   " ++ show (drDeadPositions dr)
  , "  Linear positions: " ++ show (drLinearPositions dr)
  , "  Avalanche ratio:  " ++ showPct (drAvalancheRatio dr)
      ++ avalancheGrade (drAvalancheRatio dr)
  , "  Max correlation:  " ++ showPct (drMaxCorrelation dr)
  , "  Differential:     " ++ if drIsDifferential dr then "⚠ YES" else "✓ No"
  ]

avalancheGrade :: Double -> String
avalancheGrade r
  | r >= 0.5  = " ✓ (excellent)"
  | r >= 0.3  = " (acceptable)"
  | r >= 0.1  = " ⚠ (weak diffusion)"
  | otherwise = " ☠ (critical leakage)"

showPct :: Double -> String
showPct d = show (round (d * 100) :: Int) ++ "%"
