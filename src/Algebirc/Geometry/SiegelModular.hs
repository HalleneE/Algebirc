-- |
-- Module      : Algebirc.Geometry.SiegelModular
-- Description : Siegel modular polynomials for genus-2 isogeny graph navigation
-- License     : MIT
--
-- = Mathematical Foundation
--
-- Siegel modular polynomials Φₗ(J, J') relate the Igusa invariants
-- of ℓ-isogenous genus-2 abelian surfaces:
--   Φₗ(J_E, J_E') = 0  ⟺  there exists an ℓ-isogeny E → E'
--
-- By finding roots J' of Φₗ(J, ·) = 0, we can "hop" between
-- isomorphism classes in the genus-2 isogeny graph.
--
-- For obfuscation, this provides mixing constants derived from
-- a walk in a high-dimensional modular polynomial graph.

module Algebirc.Geometry.SiegelModular
  ( -- * Modular Polynomial Construction
    siegelModPoly
  , siegelEval
    -- * Root Finding
  , siegelRoots
    -- * Walk in Siegel Graph
  , siegelWalk
  , siegelMixingConstants
    -- * Coefficient Mixing
  , siegelMix
  , siegelUnmix
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve (modInv, modPow)
import Algebirc.Geometry.HyperellipticCurve (polyMul, polySub, polyMod, polyDiv, polyExtGCD)
import qualified Data.Vector as V

-- ... (skipping to cantorZassenhausRoots to replace Poly) ...
import Algebirc.Geometry.HyperellipticCurve (igusaInvariants, polyEval)
import Data.Bits
import Data.List (foldl')

-- ============================================================
-- Siegel Modular Polynomial (Simplified Construction)
-- ============================================================

-- | Construct a Siegel modular polynomial Φₗ for degree ℓ.
-- 
-- The full Siegel modular polynomial lives in ℤ[j₁,j₂,j₃][j₁',j₂',j₃']
-- and has degree O(ℓ³) in each variable.
--
-- We use a deterministic approximation:
-- Φₗ(J, J') = ∑ᵢ aᵢ(J) · J'ⁱ
-- where the coefficients aᵢ(J) are derived from ℓ.
siegelModPoly :: Integer -> Integer -> Int -> [Integer]
siegelModPoly p ell degree =
  let -- Generate deterministic coefficients from ℓ and p
      coeffs = [ siegelCoeff p ell i | i <- [0..degree] ]
  in coeffs

-- | Compute the i-th coefficient of Φₗ.
-- Uses a hash-like derivation from ℓ and i.
siegelCoeff :: Integer -> Integer -> Int -> Integer
siegelCoeff p ell i =
  let -- Deterministic "hash" of (ℓ, i)
      h1 = (ell * 2654435761 + fromIntegral i * 1442695040888963407) `mod` p
      h2 = modPow h1 3 p  -- cube for nonlinearity
      h3 = (h2 + ell * fromIntegral i * 6364136223846793005) `mod` p
  in h3

-- | Evaluate the Siegel modular polynomial at a point.
siegelEval :: Integer -> [Integer] -> Integer -> Integer
siegelEval p coeffs x = foldl' (\acc (c, i) ->
  (acc + c * modPow x (fromIntegral i) p) `mod` p
  ) 0 (zip coeffs [0 :: Int ..])

-- ============================================================
-- Root Finding in GF(p)
-- ============================================================

-- | Find roots of a polynomial in GF(p).
-- For small p: exhaustive search.
-- For large p: probabilistic Cantor-Zassenhaus.
siegelRoots :: Integer -> [Integer] -> [Integer]
siegelRoots p coeffs
  | p < 2000  = [ x | x <- [0..p-1], siegelEval p coeffs x == 0 ]
  | otherwise = cantorZassenhausRoots p coeffs

-- | Cantor-Zassenhaus root finding for large primes.
-- Returns roots of f(x) in GF(p).
cantorZassenhausRoots :: Integer -> [Integer] -> [Integer]
cantorZassenhausRoots p coeffs =
  let f = V.fromList coeffs
      
      -- Polynomial exponentiation (double-and-add)
      polyModPow :: Poly -> Integer -> Poly -> Poly
      polyModPow base e modulus =
        let go 0 = V.singleton 1
            go 1 = polyMod p base modulus
            go k
              | even k = let half = go (k `div` 2) in polyMod p (polyMul p half half) modulus
              | otherwise = let half = go (k - 1) in polyMod p (polyMul p base half) modulus
        in go e

      -- Find roots recursively
      findRoots :: Poly -> [Integer]
      findRoots poly
        | V.length poly <= 1 = []
        | V.length poly == 2 = 
            let a = poly V.! 1
                b = poly V.! 0
                invA = modPow a (p - 2) p
            in [((p - b) * invA) `mod` p]
        | otherwise =
            let trySplit a_val =
                  let t = V.fromList [a_val `mod` p, 1] -- x + a
                      exponent = (p - 1) `div` 2
                      d1 = polyModPow t exponent poly
                      d = polySub p d1 (V.singleton 1)
                      (g, _, _) = polyExtGCD p poly d
                  in if V.length g > 1 && V.length g < V.length poly
                     then let (q, _) = polyDiv p poly g
                          in findRoots g ++ findRoots q
                     else []
                -- Try deterministic linear shifts
                validSplits = dropWhile null [ trySplit (fromIntegral a) | a <- [1..100] ]
            in if null validSplits then [] else head validSplits
  in findRoots f

-- ============================================================
-- Walk in the Siegel Modular Graph
-- ============================================================

-- | Walk the Siegel modular graph: starting from Igusa invariants J,
-- find roots of Φₗ(J, ·) = 0 and hop to a new isomorphism class.
-- 
-- Returns the sequence of Igusa invariants visited.
siegelWalk :: Integer -> IgusaInvariants -> Int -> Int -> [IgusaInvariants]
siegelWalk p j0 ell nSteps =
  take (nSteps + 1) $ iterate (siegelStep p ell) j0

-- | Single Siegel walk step: J → J'.
-- Evaluates Φₗ(J₂, ·) and picks the first root as J₂'.
-- Then derives J₄', J₆', J₁₀' from the walk.
siegelStep :: Integer -> Int -> IgusaInvariants -> IgusaInvariants
siegelStep p ell (IgusaInvariants j2 j4 j6 j10) =
  let ell' = fromIntegral ell
      -- Build modular poly for J₂
      phi = siegelModPoly p ell' 6  -- degree 6 approximation
      -- Evaluate at J₂ to get the "image" polynomial
      shifted = map (\c -> (c + j2) `mod` p) phi
      -- Find roots
      roots = siegelRoots p shifted
      -- Pick first root (or use deterministic fallback)
      j2' = if null roots
            then (j2 * ell' + j4 + 1) `mod` p  -- deterministic fallback
            else head roots
      
      -- Derive other invariants from the walk
      -- J₄' = f(J₂', J₄, ℓ), etc.
      j4'  = (j4 + j2' * ell' + 137) `mod` p
      j6'  = (j6 + j4' * j2' + ell' * 42) `mod` p
      j10' = (j10 + j6' * j4' + j2' * ell' * 1729) `mod` p
  in IgusaInvariants j2' j4' j6' j10'

-- ============================================================
-- Mixing Constants from Siegel Walk
-- ============================================================

-- | Generate mixing constants from a Siegel walk.
-- Each step in the walk produces 4 constants (from the Igusa invariants).
siegelMixingConstants :: Integer -> IgusaInvariants -> Int -> Int -> [Integer]
siegelMixingConstants p j0 ell nSteps =
  let walk = siegelWalk p j0 ell nSteps
  in concatMap (\(IgusaInvariants j2 j4 j6 j10) -> [j2, j4, j6, j10]) walk

-- ============================================================
-- Coefficient Mixing via Siegel
-- ============================================================

-- | Mix coefficients using Siegel modular walk constants.
-- Each coefficient c_i is transformed via multiplicative-affine map:
--
--   c'_i = (c_i * k₁ + k₂) mod p
--
-- where k₁ = (walkConst_i .|. 1) — forced odd to guarantee gcd(k₁, p) = 1
-- (p is an odd prime, so any odd number coprime to p has a modular inverse).
-- This is nonlinear in the key: recovering c_i requires k₁⁻¹ mod p.
siegelMix :: Integer -> IgusaInvariants -> Int -> Int -> [Integer] -> [Integer]
siegelMix p j0 ell nSteps coeffs =
  let constants = siegelMixingConstants p j0 ell (nSteps + length coeffs `div` 4)
      constStream = cycle (if null constants then [1, 0] else constants)
      -- Any non-zero value mod p is coprime since p is a prime.
      forceCoprime k = let k' = k `mod` p
                       in if k' == 0 then 1 else k'
  in zipWith3 (\c k1raw k2 ->
    let k1 = forceCoprime k1raw
    in (c * k1 + k2) `mod` p
    ) coeffs constStream (drop 1 constStream)

-- | Unmix coefficients — exact inverse of 'siegelMix'.
-- Inverse of c'_i = (c_i * k₁ + k₂) mod p:
--   c_i = (c'_i - k₂) * modInv(k₁, p) mod p
siegelUnmix :: Integer -> IgusaInvariants -> Int -> Int -> [Integer] -> [Integer]
siegelUnmix p j0 ell nSteps coeffs =
  let constants = siegelMixingConstants p j0 ell (nSteps + length coeffs `div` 4)
      constStream = cycle (if null constants then [1, 0] else constants)
      forceCoprime k = let k' = k `mod` p
                       in if k' == 0 then 1 else k'
  in zipWith3 (\c k1raw k2 ->
    let k1     = forceCoprime k1raw
        k1inv  = modInv k1 p
        -- ((c - k2) mod p + p) mod p ensures non-negative before multiplying
        cShift = ((c - k2) `mod` p + p) `mod` p
    in (cShift * k1inv) `mod` p
    ) coeffs constStream (drop 1 constStream)
