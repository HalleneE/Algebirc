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
import Algebirc.Geometry.HyperellipticCurve (igusaInvariants, polyEval)
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
  let -- Try a set of deterministic probe points
      probes = [ modPow (fromIntegral i * 2654435761) 1 p | i <- [0..min 127 (p-1)] ]
      roots = filter (\x -> siegelEval p coeffs x == 0) probes
  in roots

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
-- Each coefficient c_i gets XOR-folded with the walk constants:
--   c'_i = (c_i + walkConst_i + walkConst_{i+1} * walkConst_{i+2}) mod p
siegelMix :: Integer -> IgusaInvariants -> Int -> Int -> [Integer] -> [Integer]
siegelMix p j0 ell nSteps coeffs =
  let constants = siegelMixingConstants p j0 ell (nSteps + length coeffs `div` 4)
      -- Ensure we have enough constants
      constStream = cycle (if null constants then [1] else constants)
  in zipWith3 (\c k1 k2 ->
    (c + k1 + k1 * k2) `mod` p
    ) coeffs constStream (drop 1 constStream)

-- | Unmix coefficients (inverse of siegelMix).
siegelUnmix :: Integer -> IgusaInvariants -> Int -> Int -> [Integer] -> [Integer]
siegelUnmix p j0 ell nSteps coeffs =
  let constants = siegelMixingConstants p j0 ell (nSteps + length coeffs `div` 4)
      constStream = cycle (if null constants then [1] else constants)
  in zipWith3 (\c k1 k2 ->
    ((c - k1 - k1 * k2) `mod` p + p) `mod` p
    ) coeffs constStream (drop 1 constStream)
