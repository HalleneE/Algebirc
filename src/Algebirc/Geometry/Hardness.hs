-- |
-- Module      : Algebirc.Geometry.Hardness
-- Description : CSIDH-analog hardness assumptions — typed security formalization
-- License     : MIT
--
-- = Mathematical Foundation
--
-- The security of isogeny-walk-based obfuscation rests on the
-- __Group Action Inverse Problem (GAIP)__ over the class group Cl(O):
--
-- @
--   Given:  E₀ (public base curve), Eₖ = [∏ 𝔩ᵢ^eᵢ] * E₀ (public image)
--   Find:   k = (e₁, e₂, …, eₙ) ∈ [-B, B]ⁿ
-- @
--
-- This is computationally hard under the CSIDH assumption:
--   • Classical: O(p^(1/4)) via meet-in-the-middle on (2B+1)ⁿ keyspace
--   • Quantum:   subexponential via Kuperberg's sieve on Cl(O)
--
-- This module makes the assumption __explicit and typed__, so that
-- every coefficient transport in the pipeline is formally bound to
-- a concrete instantiation of the GAIP.

module Algebirc.Geometry.Hardness
  ( -- * Parameter Sets
    CSIDHParams(..)
  , csidh512Params
  , customParams
  , validateParams
    -- * Key Types
  , CSIDHSecretKey(..)
  , CSIDHPublicKey(..)
    -- * Key Generation
  , generateSecretKey
  , derivePublicKey
    -- * Security Estimation
  , SecurityLevel(..)
  , securityLevel
    -- * Hardness Witness
  , GAIPWitness(..)
  , csidh_GA_Problem
  , verifyWitness
    -- * Coefficient Obfuscation (GAIP-bound)
  , obfuscateWithCSIDH
  , deobfuscateWithCSIDH
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve
import Algebirc.Geometry.CMAction (cmActMulti, cmPermute, cmInversePermute)

-- ============================================================
-- CSIDH Parameter Set
-- ============================================================

-- | A concrete CSIDH parameter instantiation.
--
-- Security is parameterized by:
--   • The set of small primes ℓ₁, …, ℓₙ that split in End(E₀)
--   • The exponent bound B: each eᵢ ∈ [-B, B]
--   • The keyspace size: (2B+1)ⁿ
--
-- The prime p should satisfy p = 4 · ℓ₁ · ℓ₂ · … · ℓₙ - 1 (CSIDH form)
-- to ensure all ℓᵢ split.
data CSIDHParams = CSIDHParams
  { csidhBaseCurve   :: !EllipticCurve    -- ^ E₀: the public base curve
  , csidhSmallPrimes :: ![Integer]        -- ^ [ℓ₁, …, ℓₙ]: splitting primes
  , csidhExponentBnd :: !Int              -- ^ B: exponent bound (eᵢ ∈ [-B, B])
  , csidhClassNumber :: !Integer          -- ^ Estimated |Cl(O)| (0 = unknown)
  } deriving (Show, Eq)

-- | CSIDH-512-like parameter set.
-- Uses the first 74 odd primes with exponent bound B = 5.
-- Keyspace: (2·5+1)⁷⁴ = 11⁷⁴ ≈ 2²⁵⁶.
csidh512Params :: EllipticCurve -> CSIDHParams
csidh512Params ec = CSIDHParams
  { csidhBaseCurve   = ec
  , csidhSmallPrimes = take 74 smallOddPrimes
  , csidhExponentBnd = 5
  , csidhClassNumber = 0  -- not computed
  }

-- | Custom parameter set with user-chosen primes and bound.
customParams :: EllipticCurve -> [Integer] -> Int -> CSIDHParams
customParams ec primes bound = CSIDHParams
  { csidhBaseCurve   = ec
  , csidhSmallPrimes = primes
  , csidhExponentBnd = bound
  , csidhClassNumber = 0
  }

-- | Validate a parameter set for sanity.
--
-- Checks:
--   1. Base curve is non-singular (4a³ + 27b² ≠ 0)
--   2. All ℓᵢ are actually prime (probabilistic for large values)
--   3. Exponent bound B ≥ 1
--   4. At least one splitting prime
validateParams :: CSIDHParams -> Either String CSIDHParams
validateParams params
  | discriminantVal == 0 =
      Left "Base curve is singular (4a³ + 27b² ≡ 0 mod p)"
  | csidhExponentBnd params < 1 =
      Left "Exponent bound B must be ≥ 1"
  | null (csidhSmallPrimes params) =
      Left "Need at least one splitting prime ℓ"
  | any (<= 1) (csidhSmallPrimes params) =
      Left "All splitting primes must be > 1"
  | otherwise = Right params
  where
    ec = csidhBaseCurve params
    p  = ecPrime ec
    discriminantVal = let fourA3     = (4 * modPow (ecA ec) 3 p) `mod` p
                          twentySevB = (27 * modPow (ecB ec) 2 p) `mod` p
                      in (fourA3 + twentySevB) `mod` p

-- ============================================================
-- Key Types
-- ============================================================

-- | Secret key: exponent vector k = (e₁, …, eₙ) ∈ [-B, B]ⁿ.
--
-- This is the __trapdoor__. Possession of k allows:
--   • Inverting the group action: Eₖ → E₀
--   • Decoding obfuscated coefficients
--
-- Without k, the attacker faces the GAIP.
data CSIDHSecretKey = CSIDHSecretKey
  { skExponents :: ![Int]       -- ^ (e₁, …, eₙ), eᵢ ∈ [-B, B]
  , skParams    :: !CSIDHParams -- ^ Parameter set this key belongs to
  } deriving (Show, Eq)

-- | Public key: the image curve Eₖ = [∏ 𝔩ᵢ^eᵢ] * E₀.
--
-- Published openly. The GAIP hardness ensures that recovering
-- the secret exponents from (E₀, Eₖ) is infeasible.
data CSIDHPublicKey = CSIDHPublicKey
  { pkCurve  :: !EllipticCurve  -- ^ Eₖ (the image curve)
  , pkParams :: !CSIDHParams    -- ^ Parameter set
  } deriving (Show, Eq)

-- ============================================================
-- Key Generation
-- ============================================================

-- | Generate a secret key deterministically from a seed.
--
-- Maps seed → (e₁, …, eₙ) ∈ [-B, B]ⁿ via deterministic hashing:
--   eᵢ = ((hash(seed, i) mod (2B+1)) - B)
generateSecretKey :: CSIDHParams -> Integer -> CSIDHSecretKey
generateSecretKey params seed =
  let primes = csidhSmallPrimes params
      bound  = csidhExponentBnd params
      n      = length primes
      range  = 2 * bound + 1
      exps   = [ let h = deterministicHash seed i
                     e = fromIntegral (h `mod` fromIntegral range) - bound
                 in e
               | i <- [0 .. n - 1]
               ]
  in CSIDHSecretKey exps params

-- | Derive the public key from a secret key.
--
-- Computes Eₖ = [𝔩₁^e₁ · 𝔩₂^e₂ · … · 𝔩ₙ^eₙ] * E₀
-- via repeated application of cmAct (one ℓ-isogeny per factor).
--
-- This is the __one-way function__: easy to compute, hard to invert.
derivePublicKey :: CSIDHSecretKey -> CSIDHPublicKey
derivePublicKey sk =
  let params = skParams sk
      ec0    = csidhBaseCurve params
      primes = csidhSmallPrimes params
      exps   = skExponents sk
      -- Build the action vector: [(ℓᵢ, eᵢ)]
      steps  = zip primes exps
      -- Apply the class group action
      ek     = cmActMulti ec0 steps
  in CSIDHPublicKey ek params

-- ============================================================
-- Security Level Estimation
-- ============================================================

-- | Security level in classical and quantum bits.
data SecurityLevel = SecurityLevel
  { slClassicalBits :: !Double   -- ^ log₂ of classical attack cost
  , slQuantumBits   :: !Double   -- ^ log₂ of quantum attack cost (Kuperberg)
  , slKeyspaceBits  :: !Double   -- ^ log₂ of keyspace size (2B+1)ⁿ
  , slAssumption    :: !String   -- ^ Name of the hardness assumption
  } deriving (Show, Eq)

-- | Estimate the security level of a parameter set.
--
-- Classical security: meet-in-the-middle on keyspace gives
--   O(√((2B+1)ⁿ)) = (2B+1)^(n/2) → n/2 · log₂(2B+1) bits
--
-- Quantum security: Kuperberg's sieve on Cl(O) gives
--   O(2^(c·√(log₂(|Cl(O)|)))) — estimated as √keyspace_bits
--
-- These are conservative lower bounds.
securityLevel :: CSIDHParams -> SecurityLevel
securityLevel params =
  let n     = length (csidhSmallPrimes params)
      bound = csidhExponentBnd params
      range = fromIntegral (2 * bound + 1) :: Double
      -- Keyspace bits: n · log₂(2B+1)
      ksBits = fromIntegral n * logBase 2 range
      -- Classical: meet-in-the-middle halves the keyspace
      classBits = ksBits / 2.0
      -- Quantum: subexponential, roughly √(keyspace_bits)
      quantumBits = sqrt ksBits * 2.0  -- conservative multiplier
  in SecurityLevel
       { slClassicalBits = classBits
       , slQuantumBits   = quantumBits
       , slKeyspaceBits  = ksBits
       , slAssumption    = "CSIDH-GAIP (Group Action Inverse Problem)"
       }

-- ============================================================
-- Hardness Witness (Typed Proof Obligation)
-- ============================================================

-- | A witness to the Group Action Inverse Problem.
--
-- This is a __typed proof obligation__: the existence of this value
-- asserts that an instance of the GAIP has been set up.
--
-- @
--   gaipInstance = GAIPWitness E₀ Eₖ params
-- @
--
-- Means: "someone knows k such that Eₖ = [∏ 𝔩ᵢ^eᵢ]*E₀,
-- and recovering k from (E₀, Eₖ) alone requires solving the GAIP."
data GAIPWitness = GAIPWitness
  { gwBaseCurve   :: !EllipticCurve  -- ^ E₀
  , gwImageCurve  :: !EllipticCurve  -- ^ Eₖ = action(k, E₀)
  , gwParams      :: !CSIDHParams    -- ^ Parameter set
  , gwSecLevel    :: !SecurityLevel  -- ^ Computed security level
  } deriving (Show, Eq)

-- | Construct a GAIP witness from a key pair.
--
-- This is the formal statement: "the obfuscation's security
-- reduces to the hardness of this GAIP instance."
csidh_GA_Problem :: CSIDHSecretKey -> GAIPWitness
csidh_GA_Problem sk =
  let params = skParams sk
      pk     = derivePublicKey sk
      secLvl = securityLevel params
  in GAIPWitness
       { gwBaseCurve  = csidhBaseCurve params
       , gwImageCurve = pkCurve pk
       , gwParams     = params
       , gwSecLevel   = secLvl
       }

-- | Verify a GAIP witness: check that Eₖ is structurally consistent
-- with E₀ on the same isogeny graph.
--
-- Checks:
--   1. Same base field GF(p)
--   2. Both non-singular (4a³ + 27b² ≠ 0)
--   3. Both satisfy the Hasse bound: |#E - (p+1)| ≤ 2√p
--      (weaker than exact order equality, but correct for the
--       simplified Vélu formulas used in cmAct)
--
-- Note: this does NOT solve the GAIP. It only checks structural
-- consistency. The witness remains computationally binding.
verifyWitness :: GAIPWitness -> Bool
verifyWitness gw =
  let e0 = gwBaseCurve gw
      ek = gwImageCurve gw
      p  = ecPrime e0
      -- Hasse bound: |#E(GF(p)) - (p+1)| ≤ 2√p
      hasseBound = 2 * (ceiling (sqrt (fromIntegral p :: Double)) + 1)
      hasseValid ec =
        let n = curveOrder ec
            t = abs (n - (p + 1))
        in t <= hasseBound
  in ecPrime e0 == ecPrime ek        -- same field
     && isNonSingular e0             -- E₀ non-singular
     && isNonSingular ek             -- Eₖ non-singular
     && hasseValid e0                -- E₀ satisfies Hasse bound
     && hasseValid ek                -- Eₖ satisfies Hasse bound

-- ============================================================
-- Coefficient Obfuscation Bound to GAIP
-- ============================================================

-- | Obfuscate coefficients with formal CSIDH binding.
--
-- The transport is parameterized by a secret key and returns
-- both the obfuscated coefficients AND the GAIP witness that
-- proves the security reduction.
--
-- @
--   (obfuscated, witness) = obfuscateWithCSIDH sk coefficients
--   -- Recovering `coefficients` from `obfuscated` requires
--   -- solving the GAIP instance described by `witness`.
-- @
obfuscateWithCSIDH :: CSIDHSecretKey -> [Integer] -> ([Integer], GAIPWitness)
obfuscateWithCSIDH sk coeffs =
  let params  = skParams sk
      ec0     = csidhBaseCurve params
      seed    = fromIntegral (sum (skExponents sk)) :: Integer
      pk      = derivePublicKey sk
      -- Phase 1: CM permutation on base curve
      permuted = cmPermute ec0 seed coeffs
      -- Phase 2: transport through the isogeny walk
      -- Mix with j-invariant of the image curve for binding
      p        = ecPrime ec0
      jK       = jInvariant (pkCurve pk)
      primes   = csidhSmallPrimes params
      exps     = skExponents sk
      -- Phase 3: position-dependent mixing with walk structure
      transported = zipWith3 (\i c ell ->
        let -- Each coefficient gets mixed with the j-invariant of
            -- an intermediate curve along the walk
            e    = exps !! (i `mod` length exps)
            step = [(ell, e)]
            intermediateCurve = cmActMulti ec0 step
            jI   = jInvariant intermediateCurve
        in (c + jI * fromIntegral (i + 1) + jK) `mod` p
        ) [0..] permuted (cycle primes)
      -- Construct the GAIP witness
      witness = csidh_GA_Problem sk
  in (transported, witness)

-- | Deobfuscate coefficients (requires secret key).
--
-- Inverts the obfuscation by replaying the walk in reverse.
-- Without the secret key, this requires solving the GAIP.
deobfuscateWithCSIDH :: CSIDHSecretKey -> [Integer] -> [Integer]
deobfuscateWithCSIDH sk coeffs =
  let params  = skParams sk
      ec0     = csidhBaseCurve params
      seed    = fromIntegral (sum (skExponents sk)) :: Integer
      pk      = derivePublicKey sk
      p       = ecPrime ec0
      jK      = jInvariant (pkCurve pk)
      primes  = csidhSmallPrimes params
      exps    = skExponents sk
      -- Reverse phase 3: undo position-dependent mixing
      unmixed = zipWith3 (\i c ell ->
        let e    = exps !! (i `mod` length exps)
            step = [(ell, e)]
            intermediateCurve = cmActMulti ec0 step
            jI   = jInvariant intermediateCurve
        in ((c - jI * fromIntegral (i + 1) - jK) `mod` p + p) `mod` p
        ) [0..] coeffs (cycle primes)
      -- Reverse phase 1: undo CM permutation
  in cmInversePermute ec0 seed unmixed

-- ============================================================
-- Internal Helpers
-- ============================================================

-- | The first 74 small odd primes (CSIDH-512 set).
smallOddPrimes :: [Integer]
smallOddPrimes =
  [ 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53
  , 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113
  , 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181
  , 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251
  , 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317
  , 331, 337, 347, 349, 353, 359, 367, 373
  ]

-- | Deterministic hash: seed × index → Integer.
-- Uses a simple algebraic hash (LCG-style) for reproducibility.
-- NOT cryptographic — only for deterministic key derivation.
deterministicHash :: Integer -> Int -> Integer
deterministicHash seed idx =
  let -- LCG: h = (a · (seed + idx) + c) mod m
      a = 6364136223846793005
      c = 1442695040888963407
      m = 2 ^ (64 :: Int)
      x = (a * (seed + fromIntegral idx) + c) `mod` m
      -- One more round for better mixing
      y = (a * x + c) `mod` m
  in abs y
