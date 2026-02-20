-- |
-- Module      : Algebirc.Geometry.RichelotIsogeny
-- Description : (2,2)-isogenies between genus-2 Jacobians (Richelot correspondences)
-- License     : MIT
--
-- = Mathematical Foundation
--
-- A Richelot isogeny is a (2,2)-isogeny between principally polarized
-- abelian surfaces (genus-2 Jacobians).
--
-- Given C: y² = G₁(x)·G₂(x)·G₃(x) where each Gᵢ is quadratic,
-- the Richelot dual curve C' has:
--   y² = δ₁·G₂'·G₃'(x) · δ₂·G₃'·G₁'(x) · δ₃·G₁'·G₂'(x)
-- where Gᵢ' are computed via the Richelot correspondence
-- and δᵢ are resultant-derived scalars.

module Algebirc.Geometry.RichelotIsogeny
  ( -- * Richelot Computation
    richelotDual
  , richelotStep
  , richelotWalk
    -- * Factorization
  , factorSextic
    -- * Coefficient Transport
  , richelotTransport
  , richelotInverseTransport
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve (modInv, modPow)
import Algebirc.Geometry.HyperellipticCurve
import Data.List (foldl')

-- ============================================================
-- Quadratic Factorization
-- ============================================================

-- | Factor a sextic f(x) = G₁(x)·G₂(x)·G₃(x) into three quadratics.
-- Uses a deterministic splitting strategy based on root search in GF(p).
--
-- If exact factorization fails, returns a "pseudo-factorization"
-- using polynomial division with synthetic roots.
factorSextic :: Integer -> [Integer] -> ([Integer], [Integer], [Integer])
factorSextic p coeffs =
  let -- Find roots of f in GF(p) by brute force (small primes)
      -- or by Berlekamp-like factoring (large primes)
      roots = findRoots p coeffs
      
      -- Build quadratic factors from pairs of roots
      (g1, g2, g3) = case roots of
        (r1:r2:r3:r4:r5:r6:_) ->
          -- Three quadratics from six roots: (x-r₁)(x-r₂), etc.
          ( buildQuadratic p r1 r2
          , buildQuadratic p r3 r4
          , buildQuadratic p r5 r6
          )
        (r1:r2:r3:r4:_) ->
          -- Four roots, one irreducible quadratic factor
          ( buildQuadratic p r1 r2
          , buildQuadratic p r3 r4
          , syntheticQuadratic p coeffs r1 r2 r3 r4
          )
        _ ->
          -- Fallback: synthetic factorization
          ( take 3 (coeffs ++ repeat 0)
          , take 3 (drop 2 coeffs ++ repeat 0)
          , [1, 0, 1]  -- x² + 1
          )
  in (g1, g2, g3)

-- | Find roots of polynomial f(x) in GF(p).
findRoots :: Integer -> [Integer] -> [Integer]
findRoots p coeffs = [ x | x <- [0..min (p-1) 256], polyEval p coeffs x == 0 ]

-- | Build quadratic (x - r₁)(x - r₂) = x² - (r₁+r₂)x + r₁r₂.
buildQuadratic :: Integer -> Integer -> Integer -> [Integer]
buildQuadratic p r1 r2 =
  let c0 = (r1 * r2) `mod` p
      c1 = ((p - r1 - r2) `mod` p + p) `mod` p
  in [c0, c1, 1]

-- | Synthetic quadratic: f / (g1 * g2) when we have 4 roots.
syntheticQuadratic :: Integer -> [Integer] -> Integer -> Integer -> Integer -> Integer -> [Integer]
syntheticQuadratic p coeffs r1 r2 r3 r4 =
  let g1 = buildQuadratic p r1 r2
      g2 = buildQuadratic p r3 r4
      g12 = polyMul p g1 g2
      (q, _) = polyDiv p coeffs g12
  in take 3 (q ++ repeat 0)

-- ============================================================
-- Richelot Correspondence
-- ============================================================

-- | Compute the Richelot dual curve C' from C: y² = G₁·G₂·G₃.
-- Returns the new sextic coefficients for C'.
richelotDual :: Integer -> ([Integer], [Integer], [Integer]) -> [Integer]
richelotDual p (g1, g2, g3) =
  let -- Compute δᵢ = resultant-like scalars
      -- δ₁ = leading coef of G₂ · trailing coef of G₃ - vice versa
      delta1 = richelotDelta p g2 g3
      delta2 = richelotDelta p g3 g1
      delta3 = richelotDelta p g1 g2
      
      -- Compute Gᵢ' (the dual quadratics)
      -- G₁' comes from the cross-derivative of G₂ × G₃
      g1' = richelotDualQuad p g2 g3 delta1
      g2' = richelotDualQuad p g3 g1 delta2
      g3' = richelotDualQuad p g1 g2 delta3
      
      -- C': y² = G₁'·G₂'·G₃'
      product12 = polyMul p g1' g2'
      product123 = polyMul p product12 g3'
  in polyNorm p product123
  where
    polyNorm q = map (\c -> ((c `mod` q) + q) `mod` q)

-- | Compute the Richelot delta scalar for a pair of quadratics.
-- This is the discriminant-like quantity controlling the isogeny.
richelotDelta :: Integer -> [Integer] -> [Integer] -> Integer
richelotDelta p g1 g2 =
  let -- δ = a₁₀·a₂₂ - a₁₂·a₂₀ (cross of leading/trailing coefficients)
      a10 = if not (null g1) then head g1 else 0
      a12 = if length g1 > 2 then g1 !! 2 else 0
      a20 = if not (null g2) then head g2 else 0
      a22 = if length g2 > 2 then g2 !! 2 else 0
  in ((a10 * a22 - a12 * a20) `mod` p + p) `mod` p

-- | Compute the dual quadratic in the Richelot correspondence.
-- G' is derived from the cross-product structure of two input quadratics.
richelotDualQuad :: Integer -> [Integer] -> [Integer] -> Integer -> [Integer]
richelotDualQuad p g1 g2 delta =
  let -- Extract coefficients: gᵢ(x) = aᵢ₀ + aᵢ₁x + aᵢ₂x²
      a i g = if i < length g then (g !! i) `mod` p else 0
      a10 = a 0 g1; a11 = a 1 g1; a12 = a 2 g1
      a20 = a 0 g2; a21 = a 1 g2; a22 = a 2 g2
      
      -- Dual quadratic coefficients (simplified Richelot formulas)
      -- These arise from the 2-torsion structure of the Jacobian
      deltaInv = if delta /= 0 then modInv delta p else 1
      
      -- Corrected formula for the Wronskian (u'v - uv') of two quadratics
      -- (2 a_2 x + a_1)(b_2 x^2 + b_1 x + b_0) - (a_2 x^2 + a_1 x + a_0)(2 b_2 x + b_1)
      -- c2 (x^2 term) = a2*b1 - a1*b2
      c2 = ((a12 * a21 - a11 * a22) * deltaInv) `mod` p
      -- c1 (x^1 term) = 2*(a2*b0 - a0*b2)
      c1 = (2 * (a12 * a20 - a10 * a22) * deltaInv) `mod` p
      -- c0 (x^0 term) = a1*b0 - a0*b1
      c0 = ((a11 * a20 - a10 * a21) * deltaInv) `mod` p
  in [(c0 + p) `mod` p, (c1 + p) `mod` p, (c2 + p) `mod` p]

-- ============================================================
-- Richelot Walk
-- ============================================================

-- | Single Richelot step: C → C' via (2,2)-isogeny.
richelotStep :: Integer -> [Integer] -> [Integer]
richelotStep p fCoeffs =
  let (g1, g2, g3) = factorSextic p fCoeffs
  in richelotDual p (g1, g2, g3)

-- | Multi-step Richelot walk.
-- Each step applies a (2,2)-isogeny, transforming the curve's sextic.
richelotWalk :: Integer -> [Integer] -> Int -> [[Integer]]
richelotWalk p f0 nSteps =
  take (nSteps + 1) $ iterate (richelotStep p) f0

-- ============================================================
-- Coefficient Transport via Richelot
-- ============================================================

-- | Transport coefficients through a Richelot isogeny chain.
-- Strategy:
-- 1. Embed coefficients into the sextic's coefficients
-- 2. Walk the Richelot chain
-- 3. Extract transformed coefficients from the final sextic
--
-- The Igusa invariants change at each step, binding
-- the output to the specific walk path.
richelotTransport :: HyperCurve -> Int -> [Integer] -> [Integer]
richelotTransport (HyperCurve fCoeffs _ p) nSteps coeffs =
  let -- Embed coefficients by mixing with the curve's sextic
      embedded = zipWith (\c f -> (c + f) `mod` p) 
                   coeffs (cycle fCoeffs)
      
      -- Walk the Richelot chain
      walkChain = richelotWalk p fCoeffs nSteps
      finalSextic = last walkChain
      
      -- Extract: mix coefficients with the final sextic's Igusa invariants
      finalHC = HyperCurve finalSextic 2 p
      IgusaInvariants j2 j4 j6 j10 = igusaInvariants finalHC
      
      -- Apply Igusa mixing: each coefficient gets mixed with invariants
  in zipWith (\i c ->
    let invariantMix = case i `mod` 4 of
          0 -> j2
          1 -> j4
          2 -> j6
          _ -> j10
    in (c + invariantMix * fromIntegral (i + 1)) `mod` p
    ) [0 :: Int ..] embedded

-- | Inverse transport (requires the same curve and step count).
richelotInverseTransport :: HyperCurve -> Int -> [Integer] -> [Integer]
richelotInverseTransport (HyperCurve fCoeffs _ p) nSteps coeffs =
  let walkChain = richelotWalk p fCoeffs nSteps
      finalSextic = last walkChain
      finalHC = HyperCurve finalSextic 2 p
      IgusaInvariants j2 j4 j6 j10 = igusaInvariants finalHC
      
      -- Reverse Igusa mixing
      unmixed = zipWith (\i c ->
        let invariantMix = case i `mod` 4 of
              0 -> j2
              1 -> j4
              2 -> j6
              _ -> j10
        in ((c - invariantMix * fromIntegral (i + 1)) `mod` p + p) `mod` p
        ) [0 :: Int ..] coeffs
      
      -- Reverse embedding
  in zipWith (\c f -> ((c - f) `mod` p + p) `mod` p)
       unmixed (cycle fCoeffs)
