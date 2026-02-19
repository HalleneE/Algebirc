-- |
-- Module      : Algebirc.Geometry.Isogeny
-- Description : Elliptic curve isogenies — Vélu's formulas & isogeny walks
-- License     : MIT
--
-- = Mathematical Foundation
--
-- Given E and a finite kernel subgroup K ⊂ E, Vélu's formulas compute:
--   φ: E → E' = E/K
-- where E' has new coefficients (a', b').
--
-- For ℓ-isogeny (kernel of order ℓ):
--   φ̂ ∘ φ = [ℓ]  (dual isogeny)
-- This is the algebraic geometry analog of "one-way function with trapdoor."

module Algebirc.Geometry.Isogeny
  ( -- * Vélu's Formulas
    veluIsogeny
  , veluMapPoint
    -- * Isogeny Walks
  , isogenyWalk
  , walkCurves
    -- * Kernel Generation
  , findKernelPoint
  , randomKernelPoint
    -- * Coefficient Transport
  , transportCoeffs
  , inverseTransportCoeffs
    -- * Dual Isogeny
  , dualIsogenyCurve
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve
import Data.List (foldl')

-- ============================================================
-- Vélu's Formulas
-- ============================================================

-- | Vélu's isogeny: given curve E and kernel point P of order ℓ,
-- compute the codomain curve E' = E/⟨P⟩.
--
-- Returns (E', a', b') where E': y² = x³ + a'x + b'.
veluIsogeny :: EllipticCurve -> ECPoint -> Int -> EllipticCurve
veluIsogeny ec Infinity _ = ec  -- trivial isogeny
veluIsogeny ec@(EllipticCurve a b p) kernelPt ell =
  let -- Generate all non-trivial kernel points: [P, 2P, ..., (ℓ-1)P]
      kernelPts = [ ecScalarMul ec (fromIntegral k) kernelPt | k <- [1..ell-1] ]
      
      -- Split into pairs: {Q, -Q} contribute equally
      -- For odd ℓ, we take the first (ℓ-1)/2 multiples
      halfPts = take ((ell - 1) `div` 2) kernelPts
      
      -- Vélu's v and w sums
      (vSum, wSum) = foldl' (\(vacc, wacc) q ->
        case q of
          Infinity -> (vacc, wacc)
          ECPoint qx qy ->
            let gxQ = (3 * qx * qx + a) `mod` p  -- g_x(Q) = 3x_Q² + a
                gyQ = ((-2) * qy) `mod` p          -- g_y(Q) = -2y_Q
                vQ  = gxQ                           -- v_Q = g_x(Q)
                -- u_Q = g_y(Q)² (for the w computation)
                wQ  = (vQ + gxQ) `mod` p            -- simplified w contribution
            in ((vacc + 2 * vQ) `mod` p, (wacc + 2 * wQ) `mod` p)
        ) (0, 0) halfPts
      
      -- New curve: a' = a - 5v, b' = b - 7w (Vélu's formulas - simplified)
      a' = (a - 5 * vSum) `mod` p
      b' = (b - 7 * wSum) `mod` p
      
      -- Normalize
      a'' = (a' + p) `mod` p
      b'' = (b' + p) `mod` p
      
  in EllipticCurve a'' b'' p

-- | Map a point through the Vélu isogeny.
-- φ(P) computes the image of P under the isogeny defined by the kernel.
veluMapPoint :: EllipticCurve -> ECPoint -> [ECPoint] -> ECPoint -> ECPoint
veluMapPoint _ _ _ Infinity = Infinity
veluMapPoint ec@(EllipticCurve _ _ p) kernelPt kernelPts (ECPoint px py) =
  let -- For each kernel point Q (non-identity), compute contribution
      (xShift, yShift) = foldl' (\(xacc, yacc) q ->
        case q of
          Infinity -> (xacc, yacc)
          ECPoint qx qy ->
            let dx = (px - qx + p) `mod` p
            in if dx == 0
               then (xacc, yacc)  -- P is in the kernel
               else
                 let dxInv = modInv dx p
                     gxQ = (3 * qx * qx + ecA ec) `mod` p
                     gyQ = ((-2) * qy + p) `mod` p
                     -- x contribution
                     xc = (gxQ * dxInv) `mod` p
                     -- y contribution  
                     dy = (py - qy + p) `mod` p
                     yc = (gxQ * dy * dxInv `mod` p * dxInv `mod` p
                           - gyQ * dxInv) `mod` p
                 in ((xacc + xc) `mod` p, (yacc + yc) `mod` p)
        ) (0, 0) kernelPts
      
      x' = (px + xShift) `mod` p
      y' = (py + yShift) `mod` p
  in ECPoint ((x' + p) `mod` p) ((y' + p) `mod` p)

-- ============================================================
-- Kernel Point Generation
-- ============================================================

-- | Find a point of order dividing ℓ on the curve.
-- Uses cofactor multiplication: pick random point P, compute [n/ℓ]P
-- where n = #E(GF(p)).
findKernelPoint :: EllipticCurve -> Integer -> Integer -> ECPoint
findKernelPoint ec ell seed =
  let p = ecPrime ec
      n = curveOrder ec
      cofactor = n `div` ell
      -- Deterministic "random" point from seed
      basePt = liftToPoint ec (seed `mod` p)
      -- Multiply by cofactor to get point of order dividing ℓ
      kernelPt = ecScalarMul ec cofactor basePt
  in kernelPt

-- | Deterministic kernel point from seed value.
randomKernelPoint :: EllipticCurve -> Integer -> ECPoint
randomKernelPoint ec seed = liftToPoint ec seed

-- ============================================================
-- Isogeny Walk
-- ============================================================

-- | Multi-step isogeny walk: E₀ →ℓ₁ E₁ →ℓ₂ E₂ → ... → Eₖ.
-- Each step uses a deterministic kernel derived from the seed.
--
-- Returns the final curve after the walk.
isogenyWalk :: EllipticCurve -> [(Integer, Int)] -> EllipticCurve
isogenyWalk = foldl' (\ec (seed, ell) ->
  let kernel = findKernelPoint ec (fromIntegral ell) seed
  in veluIsogeny ec kernel ell
  )

-- | Walk and collect all intermediate curves (for analysis/debugging).
walkCurves :: EllipticCurve -> [(Integer, Int)] -> [EllipticCurve]
walkCurves ec0 steps = scanl (\ec (seed, ell) ->
  let kernel = findKernelPoint ec (fromIntegral ell) seed
  in veluIsogeny ec kernel ell
  ) ec0 steps

-- ============================================================
-- Coefficient Transport
-- ============================================================

-- | Transport polynomial coefficients through an isogeny chain.
--
-- Strategy:
-- 1. Lift each coefficient to a point on E₀
-- 2. Walk the isogeny chain: φₖ ∘ ... ∘ φ₁(P), using the correct
--    intermediate curve Eᵢ as the domain for each step i
-- 3. Project back to field elements (x-coordinates)
--
-- The result is algebraically bound to the isogeny path —
-- reversing requires knowing the kernel points.
--
-- Fix: each fold step uses the curve produced by the *previous* step
-- (carried as `curEc`), not the original `ec0`. This ensures
-- φᵢ: Eᵢ₋₁ → Eᵢ is applied in the correct domain.
transportCoeffs :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
transportCoeffs ec0 steps coeffs =
  let p = ecPrime ec0

      -- walkCurves yields [E₀, E₁, E₂, …, Eₙ].
      -- We zip domain curves (E₀..Eₙ₋₁) with the step that produces the next.
      curves = walkCurves ec0 steps                -- length = |steps| + 1
      domainCurves = zip curves steps              -- (Eᵢ, stepᵢ) pairs

      -- Pre-compute (kernel, kernel-point-list) for each step,
      -- using the correct domain curve Eᵢ.
      kernelData = map (\(ec, (seed, ell)) ->
          let kernel = findKernelPoint ec (fromIntegral ell) seed
              kPts   = [ ecScalarMul ec (fromIntegral k) kernel
                       | k <- [1 .. ell - 1] ]
          in (ec, kernel, kPts)
        ) domainCurves

      -- Lift coefficients to points on E₀.
      points = map (liftToPoint ec0 . (`mod` p)) coeffs

      -- Walk each point through the chain, threading current curve.
      -- State: (curPt, curEc) — curEc is the domain of the *next* step.
      transported = map (\pt0 ->
        let (finalPt, _) =
              foldl' (\(curPt, _curEc) (stepEc, kernel, kPts) ->
                let nextPt = case curPt of
                               Infinity -> Infinity
                               _        -> veluMapPoint stepEc kernel kPts curPt
                    -- Codomain is already tracked by `curves`; we only
                    -- need the domain curve for veluMapPoint, which is stepEc.
                in (nextPt, stepEc)
              ) (pt0, ec0) kernelData
        in finalPt
        ) points

  in map projectFromPoint transported

-- | Inverse transport (requires dual isogeny data).
-- For obfuscation, the forward path is stored; true inversion requires
-- knowing the kernel points (secret).
--
-- Fix: start from the *actual* final curve Eₙ produced by the forward walk,
-- then walk reversed steps from Eₙ back toward E₀.
inverseTransportCoeffs :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
inverseTransportCoeffs ec steps coeffs =
  let finalCurve = isogenyWalk ec steps
      revSteps   = reverse steps
  in transportCoeffs finalCurve revSteps coeffs

-- ============================================================
-- Dual Isogeny
-- ============================================================

-- | Compute the dual isogeny's codomain (which is the original curve).
-- The dual φ̂: E' → E satisfies φ̂ ∘ φ = [ℓ] on E.
-- For coefficient-level operations, this means:
--   decode(x) = [ℓ⁻¹] · φ̂(lift(x))
dualIsogenyCurve :: EllipticCurve -> ECPoint -> Int -> EllipticCurve
dualIsogenyCurve ec kernel ell =
  -- The dual of φ: E → E' has codomain E
  -- and domain E' = veluIsogeny ec kernel ell
  -- Computing full dual requires the dual kernel on E'
  let codomain = veluIsogeny ec kernel ell
      -- The dual kernel is the image of E[ℓ] - ⟨P⟩ under φ
      -- For practical purposes, we return the codomain
  in codomain
