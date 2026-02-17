-- |
-- Module      : Algebirc.Geometry.CMAction
-- Description : Complex Multiplication group action on elliptic curves
-- License     : MIT
--
-- = Mathematical Foundation
--
-- For an elliptic curve E over GF(p) with CM by an order O ⊂ ℤ[√Δ]:
--   • The class group Cl(O) acts on the set of curves with CM by O
--   • Each ideal class [𝔞] ∈ Cl(O) maps E → [𝔞]*E via an isogeny
--   • The action is free and transitive on the isomorphism classes
--
-- This gives us a "hidden group action" — computing the action is easy,
-- but finding the ideal class connecting two curves is hard.
-- (This is the CSIDH / OSIDH hardness assumption.)

module Algebirc.Geometry.CMAction
  ( -- * CM Discriminant
    cmDiscriminant
  , isSupersingular
    -- * Orientation
  , CMOrientation(..)
  , orientCurve
    -- * Group Action
  , cmAct
  , cmActMulti
    -- * Coefficient Permutation
  , cmPermute
  , cmInversePermute
    -- * Key Derivation
  , cmDeriveKey
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve
import Data.List (foldl')

-- ============================================================
-- CM Discriminant & Supersingularity
-- ============================================================

-- | Compute the CM discriminant for the endomorphism ring.
-- For a curve over GF(p), the trace of Frobenius t satisfies:
-- #E(GF(p)) = p + 1 - t, and the CM discriminant is Δ = t² - 4p.
cmDiscriminant :: EllipticCurve -> Integer
cmDiscriminant ec =
  let p = ecPrime ec
      n = curveOrder ec
      t = p + 1 - n  -- trace of Frobenius
  in t * t - 4 * p

-- | Check if a curve is supersingular: t ≡ 0 (mod p).
-- Supersingular curves have End(E) = maximal order in a quaternion algebra.
isSupersingular :: EllipticCurve -> Bool
isSupersingular ec =
  let p = ecPrime ec
      n = curveOrder ec
      t = p + 1 - n
  in t `mod` p == 0

-- ============================================================
-- CM Orientation
-- ============================================================

-- | An orientation ι: O → End(E) fixing the action direction.
data CMOrientation = CMOrientation
  { coDisc    :: !Integer   -- ^ CM discriminant Δ
  , coTraceF  :: !Integer   -- ^ Trace of Frobenius t
  , coPrime   :: !Integer   -- ^ Field prime p
  , coSeed    :: !Integer   -- ^ Deterministic seed for reproducibility
  } deriving (Show, Eq)

-- | Orient a curve (compute its CM orientation data).
orientCurve :: EllipticCurve -> Integer -> CMOrientation
orientCurve ec seed =
  let p = ecPrime ec
      n = curveOrder ec
      t = p + 1 - n
      disc = t * t - 4 * p
  in CMOrientation disc t p seed

-- ============================================================
-- CM Group Action
-- ============================================================

-- | Apply the CM group action: [𝔞] * E → E'.
-- The ideal 𝔞 is encoded as (ℓ, direction):
--   • ℓ is a small prime splitting in O
--   • direction ∈ {+1, -1} chooses which factor of (ℓ) = 𝔩 · 𝔩̄
--
-- In practice, this is an ℓ-isogeny in the chosen direction.
cmAct :: EllipticCurve -> Integer -> Int -> EllipticCurve
cmAct ec ell direction =
  let p = ecPrime ec
      -- Generate kernel point deterministically
      seed = (ell * 31337 + fromIntegral direction * 7919 + p) `mod` p
      kernel = findKernelPoint ec ell seed
      -- The direction determines which ℓ-isogeny we take
      -- +1 = forward, -1 = "conjugate" (use negated kernel)
      kernel' = if direction > 0
                then kernel
                else ecNegate p kernel
  in veluIsogeny ec kernel' (fromIntegral ell)
  where
    -- Reuse findKernelPoint from Isogeny module
    findKernelPoint ec' ell' s =
      let n = curveOrder ec'
          cofactor = n `div` ell'
          basePt = liftToPoint ec' s
      in ecScalarMul ec' cofactor basePt

    veluIsogeny ec' kernel' ell' =
      let a = ecA ec'
          b = ecB ec'
          p' = ecPrime ec'
          halfPts = take ((ell' - 1) `div` 2) 
                    [ ecScalarMul ec' (fromIntegral k) kernel' 
                    | k <- [1..ell'-1] ]
          (vSum, wSum) = foldl' (\(va, wa) q ->
            case q of
              Infinity -> (va, wa)
              ECPoint qx _ ->
                let gxQ = (3 * qx * qx + a) `mod` p'
                in ((va + 2 * gxQ) `mod` p', (wa + 2 * (gxQ + gxQ)) `mod` p')
            ) (0, 0) halfPts
          a' = ((a - 5 * vSum) `mod` p' + p') `mod` p'
          b' = ((b - 7 * wSum) `mod` p' + p') `mod` p'
      in EllipticCurve a' b' p'

-- | Apply multiple CM actions in sequence.
-- This creates a walk in the CM class group graph.
-- exponents = [(ℓ₁, e₁), (ℓ₂, e₂), ...] where eᵢ ∈ ℤ.
cmActMulti :: EllipticCurve -> [(Integer, Int)] -> EllipticCurve
cmActMulti = foldl' (\ec (ell, exponent) ->
  let direction = if exponent >= 0 then 1 else -1
      absExp = abs exponent
  in iterate (\e -> cmAct e ell direction) ec !! absExp
  )

-- ============================================================
-- Coefficient Permutation via CM Action
-- ============================================================

-- | Permute coefficients using the CM action on j-invariants.
-- Each coefficient c_i gets mapped to:
--   c'_i = (c_i + j([𝔞ᵢ]*E)) mod p
-- where 𝔞ᵢ depends on the position i.
--
-- This mixes positional information with geometric invariants.
cmPermute :: EllipticCurve -> Integer -> [Integer] -> [Integer]
cmPermute ec seed coeffs =
  let p = ecPrime ec
      n = length coeffs
  in zipWith (\i c ->
    let -- Derive a unique ideal class from position + seed
        ell = smallPrimeAt i seed
        actedCurve = cmAct ec ell (if even i then 1 else -1)
        j = jInvariant actedCurve
    in (c + j) `mod` p
    ) [0..n-1] coeffs

-- | Inverse permutation: subtract j-invariants.
cmInversePermute :: EllipticCurve -> Integer -> [Integer] -> [Integer]
cmInversePermute ec seed coeffs =
  let p = ecPrime ec
      n = length coeffs
  in zipWith (\i c ->
    let ell = smallPrimeAt i seed
        actedCurve = cmAct ec ell (if even i then 1 else -1)
        j = jInvariant actedCurve
    in ((c - j) `mod` p + p) `mod` p
    ) [0..n-1] coeffs

-- | Derive a small prime from position index and seed.
smallPrimeAt :: Int -> Integer -> Integer
smallPrimeAt idx seed =
  let primes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
      hash = (fromIntegral idx * 6364136223846793005 + seed) `mod` fromIntegral (length primes)
  in primes !! fromIntegral hash

-- | Derive an obfuscation key from a CM walk.
-- Takes the j-invariant of the final curve as the key material.
cmDeriveKey :: EllipticCurve -> [(Integer, Int)] -> Integer
cmDeriveKey ec steps =
  let finalCurve = cmActMulti ec steps
  in jInvariant finalCurve
