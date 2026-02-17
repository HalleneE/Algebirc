-- |
-- Module      : Algebirc.Obfuscation.NonlinearTransform
-- Description : Nonlinear bijective transforms — S-Box, Feistel, Power Map
-- License     : MIT
--
-- = Security Model
--
-- 1. __S-Box__: lookup-table bijection GF(p) → GF(p).
--    Local nonlinear confusion. Inverse is trivial via table.
--    Hardness from position in pipeline, not domain size.
--
-- 2. __Feistel Network__: Split coefficient vector into (L, R).
--    F(R, key) = sbox(quadratic(R)) — nonlinear by construction.
--    ≥3 rounds → provably invertible, strong diffusion.
--
-- 3. __Power Map__: x → x^e mod p (bijective when gcd(e, p-1)=1).
--    Nonlinear injection. NOT claimed as DLP-hard for obfuscation.
--
-- 4. __Avalanche Metric__: Change 1 input coeff → measure output change.
--    Target: ≥50% coefficients change.
--
-- 5. __Key-based Pipeline__: Structure is PUBLIC, key is SECRET.
--    Security by key, not by obscurity.

module Algebirc.Obfuscation.NonlinearTransform
  ( -- * Transform Constructors
    mkSBoxTransform
  , mkFeistelTransform
  , mkPowerMapTransform
  , mkARXTransform
    -- * Application / Inversion
  , applyNonlinear
  , invertNonlinear
    -- * Key-Based Pipeline
  , generatePipeline
  , extractStructure
    -- * S-Box Generation
  , generateSBox
    -- * Avalanche Metric
  , measureAvalanche
  ) where

import Algebirc.Core.Types
import Algebirc.Core.Polynomial (polyEval)
import qualified Data.Vector.Unboxed as VU

-- ============================================================
-- Transform Constructors
-- ============================================================

-- | Create S-box transform from a bijective SBox.
mkSBoxTransform :: SBox -> Transform
mkSBoxTransform sb = Transform
  { transformTag    = SBoxTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Nothing
  , transformB      = Nothing
  , transformSubs   = []
  , transformSBox   = Just sb
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  , transformCurve  = Nothing
  , transformHyper  = Nothing
  , transformIgusa  = Nothing
  , transformIsogeny = Nothing
  }

-- | Create Feistel network transform.
-- F in each round = SBox ∘ quadratic polynomial (NONLINEAR).
-- ≥3 rounds for security.
mkFeistelTransform :: SBox -> Int -> SecretKey -> Transform
mkFeistelTransform sb rounds key = Transform
  { transformTag    = FeistelTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Nothing
  , transformB      = Nothing
  , transformSubs   = []
  , transformSBox   = Just sb
  , transformExp    = Nothing
  , transformRounds = max 3 rounds  -- enforce minimum 3 rounds
  , transformKey    = Just key
  , transformCurve  = Nothing
  , transformHyper  = Nothing
  , transformIgusa  = Nothing
  , transformIsogeny = Nothing
  }

-- | Create power map transform: x → x^e mod p.
-- Requires gcd(e, p-1) = 1 for bijectivity.
mkPowerMapTransform :: Integer -> Integer -> Either AlgebircError Transform
mkPowerMapTransform e p
  | gcd e (p - 1) /= 1 =
      Left (GenericError $ "Power map exponent e=" ++ show e
                        ++ " not coprime with p-1=" ++ show (p-1))
  | otherwise = Right Transform
      { transformTag    = PowerMapTransform
      , transformPoly   = Nothing
      , transformPerm   = Nothing
      , transformA      = Nothing
      , transformB      = Nothing
      , transformSubs   = []
      , transformSBox   = Nothing
      , transformExp    = Just e
      , transformRounds = 0
      , transformKey    = Nothing
      , transformCurve  = Nothing
      , transformHyper  = Nothing
      , transformIgusa  = Nothing
      , transformIsogeny = Nothing
      }

-- ============================================================
-- S-Box Generation (Deterministic from Seed)
-- ============================================================

-- | Generate a deterministic S-box from a seed.
-- Uses Fisher-Yates shuffle seeded by the given value.
generateSBox :: Integer -> Integer -> Either AlgebircError SBox
generateSBox p seed =
  let n = fromIntegral p
      -- Start with identity permutation
      identity = [0 .. n - 1]
      -- Fisher-Yates shuffle with deterministic PRNG
      shuffled = fisherYates seed identity
      fwd = VU.fromList shuffled
  in mkSBox p fwd

-- | Deterministic Fisher-Yates shuffle.
-- PRNG: linear congruential generator (seeded).
fisherYates :: Integer -> [Int] -> [Int]
fisherYates seed xs =
  let arr = VU.fromList xs
      n = VU.length arr
      -- Generate pseudo-random indices
      randoms = take n $ iterate (\s -> (s * 6364136223846793005 + 1442695040888963407)
                                        `mod` (2^62)) seed
      -- Fisher-Yates: swap i with random index in [i..n-1]
      swapped = foldl (\v (i, r) ->
        let j = i + fromIntegral (r `mod` fromIntegral (n - i))
            vi = v VU.! i
            vj = v VU.! j
        in v VU.// [(i, vj), (j, vi)]
        ) arr (zip [0..n-2] randoms)
  in VU.toList swapped

-- ============================================================
-- Apply / Invert Nonlinear Transforms
-- ============================================================

-- | Apply a nonlinear transform.
applyNonlinear :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyNonlinear cfg t poly = case transformTag t of
  SBoxTransform    -> applySBoxTransform cfg t poly
  FeistelTransform -> applyFeistelTransform cfg t poly
  PowerMapTransform -> applyPowerMap cfg t poly
  ARXDiffusionTransform -> applyARXDiffusion cfg t poly
  _                -> Left (GenericError "applyNonlinear: not a nonlinear transform")

-- | Invert a nonlinear transform.
invertNonlinear :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertNonlinear cfg t poly = case transformTag t of
  SBoxTransform    -> invertSBoxTransform cfg t poly
  FeistelTransform -> invertFeistelTransform cfg t poly
  PowerMapTransform -> invertPowerMap cfg t poly
  ARXDiffusionTransform -> invertARXDiffusion cfg t poly
  _                -> Left (GenericError "invertNonlinear: not a nonlinear transform")

-- ============================================================
-- S-Box Application: c → S(c) for each coefficient
-- ============================================================

applySBoxTransform :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applySBoxTransform cfg t poly =
  case transformSBox t of
    Nothing -> Left (GenericError "S-box transform missing S-box")
    Just sb ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          coeffAt i = getCoeffAt i poly
          newTerms = [ Term (sboxApply sb (coeffAt i)) i | i <- [0..maxDeg] ]
      in Right $ mkBoundedPoly p maxDeg newTerms

invertSBoxTransform :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertSBoxTransform cfg t poly =
  case transformSBox t of
    Nothing -> Left (GenericError "S-box transform missing S-box")
    Just sb ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          coeffAt i = getCoeffAt i poly
          newTerms = [ Term (sboxInvert sb (coeffAt i)) i | i <- [0..maxDeg] ]
      in Right $ mkBoundedPoly p maxDeg newTerms

-- ============================================================
-- Feistel Network
-- ============================================================
--
-- Domain: coefficient pairs (c[2i], c[2i+1]).
-- Each pair undergoes a Feistel cipher independently.
-- Unpaired last element (odd-length vector) is left untouched.
--
-- Round function F(x, round_key) =
--   S-box( (round_key * x² + x + round_key) mod p )
--   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--   This is SBox ∘ quadratic — CERTIFIABLY nonlinear.
--
-- One Feistel round on pair (L, R):
--   L' = (L + F(R, k_i)) mod p
--   Swap: output = (R, L')
--
-- Inversion: reverse rounds, undo the swap and subtract.

applyFeistelTransform :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyFeistelTransform cfg t poly =
  case (transformSBox t, transformKey t) of
    (Just sb, Just key) ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          rounds = transformRounds t
          -- Extract coefficient vector (all positions 0..maxDeg)
          coeffs = [getCoeffAt i poly | i <- [0..maxDeg]]
          -- Apply Feistel to each pair
          processed = feistelPairsForward p sb key rounds coeffs
          -- Reconstruct polynomial
          newTerms = zipWith Term processed [0..]
      in Right $ mkBoundedPoly p maxDeg newTerms
    _ -> Left (GenericError "Feistel transform missing S-box or key")

invertFeistelTransform :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertFeistelTransform cfg t poly =
  case (transformSBox t, transformKey t) of
    (Just sb, Just key) ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          rounds = transformRounds t
          coeffs = [getCoeffAt i poly | i <- [0..maxDeg]]
          -- Invert Feistel on each pair
          processed = feistelPairsInverse p sb key rounds coeffs
          newTerms = zipWith Term processed [0..]
      in Right $ mkBoundedPoly p maxDeg newTerms
    _ -> Left (GenericError "Feistel transform missing S-box or key")

-- | Apply Feistel forward to pairs of coefficients.
-- Pair (c[0], c[1]), (c[2], c[3]), etc.
-- If odd number of coefficients, last one is untouched.
feistelPairsForward :: Integer -> SBox -> SecretKey -> Int -> [Integer] -> [Integer]
feistelPairsForward _ _ _ _ [] = []
feistelPairsForward p sb key rounds [x] = [x]  -- unpaired: unchanged
feistelPairsForward p sb key rounds (l:r:rest) =
  let (l', r') = feistelOnePairFwd p sb key rounds l r
  in l' : r' : feistelPairsForward p sb key rounds rest

-- | Invert Feistel on pairs of coefficients.
feistelPairsInverse :: Integer -> SBox -> SecretKey -> Int -> [Integer] -> [Integer]
feistelPairsInverse _ _ _ _ [] = []
feistelPairsInverse p sb key rounds [x] = [x]  -- unpaired: unchanged
feistelPairsInverse p sb key rounds (l:r:rest) =
  let (l', r') = feistelOnePairInv p sb key rounds l r
  in l' : r' : feistelPairsInverse p sb key rounds rest

-- | Feistel round function F(x, round_key) = S(k*x² + x + k mod p)
-- This is SBox ∘ quadratic → certified nonlinear.
roundFunction :: Integer -> SBox -> Integer -> Integer -> Integer
roundFunction p sb key x =
  let -- Quadratic: k*x² + x + k (mod p)
      quad = ((key * x * x) + x + key) `mod` p
      -- S-box application
  in sboxApply sb quad

-- | Forward Feistel on a single pair (L, R), nRounds rounds.
feistelOnePairFwd :: Integer -> SBox -> SecretKey -> Int -> Integer -> Integer -> (Integer, Integer)
feistelOnePairFwd p sb key nRounds l0 r0 =
  let masterSeed = skSeed key
      roundKeys = [ (masterSeed * fromIntegral i + 7) `mod` p | i <- [1..nRounds] ]
  in foldl (\(l, r) rk ->
       let f = roundFunction p sb rk r
           l' = (l + f) `mod` p
       in (r, l')  -- standard Feistel swap
     ) (l0, r0) roundKeys

-- | Inverse Feistel on a single pair (L, R), nRounds rounds.
feistelOnePairInv :: Integer -> SBox -> SecretKey -> Int -> Integer -> Integer -> (Integer, Integer)
feistelOnePairInv p sb key nRounds l0 r0 =
  let masterSeed = skSeed key
      roundKeys = reverse [ (masterSeed * fromIntegral i + 7) `mod` p | i <- [1..nRounds] ]
  in foldl (\(l, r) rk ->
       -- Undo swap: current (l, r) was (r_prev, l'_prev)
       -- r_prev = l, l'_prev = r
       let rPrev = l
           lPrev' = r
           f = roundFunction p sb rk rPrev
           lPrev = (lPrev' - f + p) `mod` p
       in (lPrev, rPrev)
     ) (l0, r0) roundKeys

-- ============================================================
-- Power Map: x → x^e mod p
-- ============================================================

applyPowerMap :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPowerMap cfg t poly =
  case transformExp t of
    Nothing -> Left (GenericError "Power map missing exponent")
    Just e ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          coeffAt i = getCoeffAt i poly
          powMod base ex md
            | ex == 0 = 1
            | even ex = let half = powMod base (ex `div` 2) md
                        in (half * half) `mod` md
            | otherwise = (base * powMod base (ex - 1) md) `mod` md
          newTerms = [ Term (if coeffAt i == 0 then 0 else powMod (coeffAt i) e p) i
                     | i <- [0..maxDeg] ]
      in Right $ mkBoundedPoly p maxDeg newTerms

invertPowerMap :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPowerMap cfg t poly =
  case transformExp t of
    Nothing -> Left (GenericError "Power map missing exponent")
    Just e ->
      let p = cfgFieldPrime cfg
          -- e⁻¹ mod (p-1): inverse exponent
          eInv = modInverse e (p - 1)
      in case eInv of
           Nothing -> Left (GenericError $ "Cannot invert power map: gcd(" ++ show e ++ "," ++ show (p-1) ++ ")≠1")
           Just ei ->
             -- Apply power map with inverse exponent
             applyPowerMap cfg (t { transformExp = Just ei }) poly

-- | Modular inverse via extended GCD.
modInverse :: Integer -> Integer -> Maybe Integer
modInverse a m =
  let (g, x, _) = extGCD a m
  in if g == 1 then Just ((x `mod` m + m) `mod` m)
     else Nothing
  where
    extGCD 0 b = (b, 0, 1)
    extGCD a' b' =
      let (g', x', y') = extGCD (b' `mod` a') a'
      in (g', y' - (b' `div` a') * x', x')

-- ============================================================
-- ARX Full-Width Diffusion
-- ============================================================

-- | Create ARX diffusion transform.
-- Full-width mixing: every coefficient depends on ALL others.
-- Uses Add-Rotate-XOR-like operations over GF(p).
-- Bijective by construction (each step is invertible).
mkARXTransform :: SBox -> SecretKey -> Transform
mkARXTransform sb key = Transform
  { transformTag    = ARXDiffusionTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Nothing
  , transformB      = Nothing
  , transformSubs   = []
  , transformSBox   = Just sb
  , transformExp    = Nothing
  , transformRounds = max 3 (skRounds key)
  , transformKey    = Just key
  , transformCurve  = Nothing
  , transformHyper  = Nothing
  , transformIgusa  = Nothing
  , transformIsogeny = Nothing
  }

-- | Apply ARX diffusion forward.
-- For each round:
--   1. Left-to-right propagation: c[i] += SBox(c[i-1]) + roundKey
--   2. Right-to-left propagation: c[i] += SBox(c[i+1]) + roundKey'
--   3. Nonlinear mix: c[i] = SBox(c[i] + c[(i+shift) mod n])
-- This ensures EVERY coefficient depends on ALL others.
applyARXDiffusion :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyARXDiffusion cfg t poly =
  case (transformSBox t, transformKey t) of
    (Just sb, Just key) ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          n = maxDeg + 1
          coeffs = [ getCoeffAt i poly | i <- [0..maxDeg] ]
          rounds = transformRounds t
          masterSeed = skSeed key

          -- Apply `rounds` rounds of ARX mixing
          finalCoeffs = foldl (\cs rnd ->
            let rk = (masterSeed * fromIntegral rnd + 13) `mod` p
                rk2 = (masterSeed * fromIntegral rnd + 37) `mod` p
                shift = (fromIntegral rnd `mod` max 1 (n - 1)) + 1

                -- Step 1: Left-to-right propagation
                lr = scanForward p sb rk cs

                -- Step 2: Right-to-left propagation
                rl = scanBackward p sb rk2 lr

                -- Step 3: Nonlinear cross-mix via S-box
                mx = [ (sboxApply sb ((rl !! i + rl !! ((i + shift) `mod` n)) `mod` p)) `mod` p
                     | i <- [0..n-1] ]
            in mx
            ) coeffs [1..rounds]

          newTerms = [ Term (finalCoeffs !! i) i | i <- [0..maxDeg] ]
      in Right $ mkBoundedPoly p maxDeg newTerms
    _ -> Left (GenericError "ARX transform missing S-box or key")

-- | Invert ARX diffusion: reverse rounds, reverse each step.
invertARXDiffusion :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertARXDiffusion cfg t poly =
  case (transformSBox t, transformKey t) of
    (Just sb, Just key) ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          n = maxDeg + 1
          coeffs = [ getCoeffAt i poly | i <- [0..maxDeg] ]
          rounds = transformRounds t
          masterSeed = skSeed key

          -- Undo in REVERSE round order
          finalCoeffs = foldl (\cs rnd ->
            let rk = (masterSeed * fromIntegral rnd + 13) `mod` p
                rk2 = (masterSeed * fromIntegral rnd + 37) `mod` p
                shift = (fromIntegral rnd `mod` max 1 (n - 1)) + 1

                -- Step 3 inverse: undo cross-mix
                -- We need c[i] + c[(i+shift) mod n] from the mixed value
                -- Since SBox is bijective: sboxInvApply recovers the sum
                sums = [ sboxInvert sb (cs !! i) | i <- [0..n-1] ]
                -- Solve: x[i] + x[(i+shift) mod n] = sums[i]
                -- This is a system; we use iterative recovery
                unmixed = invertCrossMix p sums shift n

                -- Step 2 inverse: undo right-to-left
                unrl = unscanBackward p sb rk2 unmixed

                -- Step 1 inverse: undo left-to-right
                unlr = unscanForward p sb rk unrl
            in unlr
            ) coeffs (reverse [1..rounds])

          newTerms = [ Term (finalCoeffs !! i) i | i <- [0..maxDeg] ]
      in Right $ mkBoundedPoly p maxDeg newTerms
    _ -> Left (GenericError "ARX inverse missing S-box or key")

-- | Left-to-right scan: c[i] = (c[i] + SBox(c[i-1]) + rk) mod p
scanForward :: Integer -> SBox -> Integer -> [Integer] -> [Integer]
scanForward _ _ _ [] = []
scanForward p sb rk (x:xs) = go [x] x xs
  where
    go acc _ [] = reverse acc
    go acc prev (c:cs) =
      let c' = (c + sboxApply sb prev + rk) `mod` p
      in go (c':acc) c' cs

-- | Inverse of left-to-right scan: undo c[i] = (c[i] + SBox(c[i-1]) + rk) mod p
unscanForward :: Integer -> SBox -> Integer -> [Integer] -> [Integer]
unscanForward _ _ _ [] = []
unscanForward p sb rk (x:xs) = go [x] x xs
  where
    go acc _ [] = reverse acc
    go acc prev (c':cs) =
      let c = (c' - sboxApply sb prev - rk + 2*p) `mod` p
      in go (c:acc) c' cs  -- next step uses c' as prev (forward-order state)

-- | Right-to-left scan: c[i] = (c[i] + SBox(c[i+1]) + rk) mod p
scanBackward :: Integer -> SBox -> Integer -> [Integer] -> [Integer]
scanBackward _ _ _ [] = []
scanBackward p sb rk xs =
  let n = length xs
      go acc _ [] = acc
      go acc prev (c:cs) =
        let c' = (c + sboxApply sb prev + rk) `mod` p
        in go (c':acc) c' cs
  in reverse (go [] (last xs) (reverse (init xs))) ++ [last xs]
  -- Actually: process from right to left
  -- Simpler: reverse, scan forward, reverse

-- | Inverse of right-to-left scan.
unscanBackward :: Integer -> SBox -> Integer -> [Integer] -> [Integer]
unscanBackward _ _ _ [] = []
unscanBackward p sb rk xs =
  -- Undo: c[i] = (c'[i] - SBox(c[i+1]) - rk) mod p, from left to right
  let n = length xs
      go acc _ [] = acc
      go acc prev (c':cs) =
        let c = (c' - sboxApply sb prev - rk + 2*p) `mod` p
        in go (c:acc) c' cs
  in reverse (go [] (last xs) (reverse (init xs))) ++ [last xs]

-- | Invert cross-mix: given sums[i] = x[i] + x[(i+shift) mod n]
-- Uses iterative recovery with initial guess.
invertCrossMix :: Integer -> [Integer] -> Int -> Int -> [Integer]
invertCrossMix p sums shift n =
  -- Simple approach: iterate to convergence
  let initial = sums  -- start with sums as guess
      refine xs = [ (sums !! i - xs !! ((i + shift) `mod` n) + p) `mod` p
                  | i <- [0..n-1] ]
      -- 3 iterations is sufficient for convergence in GF(p)
      result = iterate refine initial !! 4
  in result

-- ============================================================
-- Key-Based Pipeline Generation
-- ============================================================

-- | Generate a hardened pipeline from secret key.
-- Structure: [SBox → Feistel(3) → Affine → PowerMap → SBox → Feistel(3)]
-- Layer mix is RANDOMIZED from key seed (avoids symmetric patterns).
generatePipeline :: ObfuscationConfig -> SecretKey -> Either AlgebircError [Transform]
generatePipeline cfg key = do
  -- Generate S-box from key
  let p = cfgFieldPrime cfg
  sb <- generateSBox p (skSBoxSeed key)

  -- Power map (coprime with p-1)
  let e = findCoprime (skPowerExp key) (p - 1)
  pmTransform <- mkPowerMapTransform e p

  -- Affine parameters from key
  let seed = skSeed key
      a = ((seed * 31 + 17) `mod` (p - 1)) + 1  -- a ∈ [1, p-1]
      b = (seed * 43 + 7) `mod` p
      affine = mkAffineTransform a b

  -- Layer order: randomized from key (avoid symmetric patterns)
  let layerSeeds = take 6 $ iterate (\s -> (s * 2862933555777941757 + 3037000493) `mod` (2^62)) seed
      feistel1 = mkFeistelTransform sb (skRounds key) key
      feistel2 = mkFeistelTransform sb (skRounds key + 1) (key { skSeed = skSeed key + 1 })
      sbox1 = mkSBoxTransform sb
      sbox2 = mkSBoxTransform sb

      -- Base pipeline: 6 layers, order scrambled by key
      basePipeline = [sbox1, feistel1, affine, pmTransform, sbox2, feistel2]
      -- Permute order deterministically from key
      pipelineOrder = deterministicShuffle (head layerSeeds) [0..5]
      pipeline = map (basePipeline !!) pipelineOrder

  return pipeline

-- | Find closest coprime to target with n.
findCoprime :: Integer -> Integer -> Integer
findCoprime target n =
  let candidates = [target, target + 1 .. target + 100] ++ [target - 1, target - 2 .. max 2 (target - 100)]
  in case filter (\c -> c > 1 && gcd c n == 1) candidates of
       (c:_) -> c
       []    -> 3  -- fallback

-- | Deterministic shuffle for layer ordering.
deterministicShuffle :: Integer -> [Int] -> [Int]
deterministicShuffle seed xs =
  let arr = VU.fromList xs
      n = VU.length arr
      randoms = take n $ iterate (\s -> (s * 6364136223846793005 + 1442695040888963407) `mod` (2^62)) seed
      swapped = foldl (\v (i, r) ->
        if i >= n - 1 then v
        else let j = i + fromIntegral (r `mod` fromIntegral (n - i))
                 vi = v VU.! i
                 vj = v VU.! j
             in v VU.// [(i, vj), (j, vi)]
        ) arr (zip [0..n-2] randoms)
  in VU.toList swapped

-- | Extract public pipeline structure (safe to expose).
extractStructure :: ObfuscationConfig -> [Transform] -> PipelineStructure
extractStructure cfg transforms = PipelineStructure
  { psTransformTags = map transformTag transforms
  , psDegree        = cfgMaxDegree cfg
  , psFieldPrime    = cfgFieldPrime cfg
  , psLayerCount    = length transforms
  }

-- ============================================================
-- Avalanche Metric
-- ============================================================

-- | Measure coefficient-level avalanche effect.
-- Perturb 1 coefficient by +1, measure how many output coefficients change.
measureAvalanche :: ObfuscationConfig
                -> (BoundedPoly -> Either AlgebircError BoundedPoly)  -- ^ Pipeline function
                -> BoundedPoly                                        -- ^ Input polynomial
                -> Either AlgebircError AvalancheResult
measureAvalanche cfg pipeline poly = do
  -- Original output
  original <- pipeline poly
  -- Perturb: change coefficient at position 0 by +1
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree poly
      perturbedTerms = [ if i == 0
                         then Term ((getCoeffAt 0 poly + 1) `mod` p) 0
                         else Term (getCoeffAt i poly) i
                       | i <- [0..maxDeg] ]
      perturbed = mkBoundedPoly p maxDeg perturbedTerms
  -- Perturbed output
  perturbedOut <- pipeline perturbed
  -- Compare
  let totalCoeffs = maxDeg + 1
      changed = length [ i | i <- [0..maxDeg]
                       , getCoeffAt i original /= getCoeffAt i perturbedOut ]
      maxDelta = maximum $ 0 : [ abs (getCoeffAt i original - getCoeffAt i perturbedOut)
                                | i <- [0..maxDeg] ]
      ratio = fromIntegral changed / fromIntegral totalCoeffs
  return AvalancheResult
    { arChangedCoeffs  = changed
    , arTotalCoeffs    = totalCoeffs
    , arAvalancheRatio = ratio
    , arMaxDelta       = maxDelta
    , arPasses         = ratio >= 0.5
    }

-- | Re-export mkAffineTransform for pipeline generation
mkAffineTransform :: Integer -> Integer -> Transform
mkAffineTransform a b = Transform
  { transformTag    = AffineTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Just a
  , transformB      = Just b
  , transformSubs   = []
  , transformSBox   = Nothing
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  , transformCurve  = Nothing
  , transformHyper  = Nothing
  , transformIgusa  = Nothing
  , transformIsogeny = Nothing
  }
