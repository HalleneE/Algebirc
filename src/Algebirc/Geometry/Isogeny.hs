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
  ( -- * Mixing
    isogenyMix
  , isogenyUnmix
    -- * Vélu's Formulas
  , veluIsogeny
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

-- | Vélu's isogeny: given curve E and kernel point P of exact order ℓ,
-- compute the codomain curve E' = E/⟨P⟩.
veluIsogeny :: EllipticCurve -> ECPoint -> Int -> EllipticCurve
veluIsogeny ec Infinity _ = ec  -- trivial isogeny
veluIsogeny ec@(EllipticCurve a b p) kernelPt ell =
  let halfPts = [ ecScalarMul ec (fromIntegral k) kernelPt
                | k <- [1 .. (ell - 1) `div` 2] ]
      (vSum, wSum) = foldl' (\(vacc, wacc) q ->
        case q of
          Infinity  -> (vacc, wacc)
          ECPoint qx qy ->
            let xQ2 = (qx * qx) `mod` p
                vQ  = (3 * xQ2 + a)    `mod` p
                uQ  = (2 * qy * qy)    `mod` p
                wQ  = (uQ + qx * vQ)   `mod` p
            in ((vacc + 2 * vQ) `mod` p, (wacc + 2 * wQ) `mod` p)
        ) (0, 0) halfPts
      a' = ((a - 5 * vSum) `mod` p + p) `mod` p
      b' = ((b - 7 * wSum) `mod` p + p) `mod` p
  in EllipticCurve a' b' p

-- | Map a point through the Vélu isogeny.
veluMapPoint :: EllipticCurve -> ECPoint -> [ECPoint] -> ECPoint -> ECPoint
veluMapPoint _ _ _ Infinity = Infinity
veluMapPoint ec@(EllipticCurve _ _ p) kernelPt kernelPts (ECPoint px py) =
  let (xShift, yShift) = foldl' (\(xacc, yacc) q ->
        case q of
          Infinity -> (xacc, yacc)
          ECPoint qx qy ->
            let dx = (px - qx + p) `mod` p
            in if dx == 0
               then (xacc, yacc)
               else
                 let dxInv = modInv dx p
                     gxQ = (3 * qx * qx + ecA ec) `mod` p
                     gyQ = ((-2) * qy + p) `mod` p
                     xc = (gxQ * dxInv) `mod` p
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

findKernelPoint :: EllipticCurve -> Integer -> Integer -> ECPoint
findKernelPoint ec ell seed
  | n `mod` ell /= 0 = Infinity
  | cofactor == 0     = Infinity
  | otherwise         = tryFind seed (fromIntegral p)
  where
    p        = ecPrime ec
    n        = curveOrder ec
    cofactor = n `div` ell
    tryFind _ 0 = Infinity
    tryFind s remaining =
      let basePt   = liftToPoint ec (s `mod` p)
          cofPt    = ecScalarMul ec cofactor basePt
          isNonTrivial = cofPt /= Infinity
          killsAtEll   = ecScalarMul ec ell cofPt == Infinity
      in if isNonTrivial && killsAtEll
         then cofPt
         else tryFind (s + 1) (remaining - 1)

randomKernelPoint :: EllipticCurve -> Integer -> ECPoint
randomKernelPoint ec seed = liftToPoint ec seed

-- ============================================================
-- Isogeny Walks
-- ============================================================

isogenyWalk :: EllipticCurve -> [(Integer, Int)] -> EllipticCurve
isogenyWalk = foldl' (\ec (seed, ell) ->
  let kernel = findKernelPoint ec (fromIntegral ell) seed
  in veluIsogeny ec kernel ell
  )

walkCurves :: EllipticCurve -> [(Integer, Int)] -> [EllipticCurve]
walkCurves ec0 steps = scanl (\ec (seed, ell) ->
  let kernel = findKernelPoint ec (fromIntegral ell) seed
  in veluIsogeny ec kernel ell
  ) ec0 steps

-- ============================================================
-- Mixing
-- ============================================================

-- | Mix coefficients via isogeny walk invariants.
isogenyMix :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
isogenyMix ec0 steps coeffs =
  let p = ecPrime ec0
      curves = walkCurves ec0 steps
      jInvariants = map jInvariant curves
      constStream = cycle (if null jInvariants then [1] else jInvariants)
  in zipWith (\c j -> (c + j) `mod` p) coeffs constStream

-- | Unmix coefficients (inverse of isogenyMix).
isogenyUnmix :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
isogenyUnmix ec0 steps coeffs =
  let p = ecPrime ec0
      curves = walkCurves ec0 steps
      jInvariants = map jInvariant curves
      constStream = cycle (if null jInvariants then [1] else jInvariants)
  in zipWith (\c j -> ((c - j) `mod` p + p) `mod` p) coeffs constStream

-- ============================================================
-- Coefficient Transport
-- ============================================================

transportCoeffs :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
transportCoeffs ec0 steps coeffs =
  let p = ecPrime ec0
      curves = walkCurves ec0 steps
      domainCurves = zip curves steps
      kernelData = map (\(ec, (seed, ell)) ->
          let kernel = findKernelPoint ec (fromIntegral ell) seed
              kPts   = [ ecScalarMul ec (fromIntegral k) kernel
                       | k <- [1 .. ell - 1] ]
          in (ec, kernel, kPts)
        ) domainCurves
      points = map (liftToPoint ec0 . (`mod` p)) coeffs
      transported = map (\pt0 ->
        let (finalPt, _) =
              foldl' (\(curPt, _curEc) (stepEc, kernel, kPts) ->
                let nextPt = case curPt of
                               Infinity -> Infinity
                               _        -> veluMapPoint stepEc kernel kPts curPt
                in (nextPt, stepEc)
              ) (pt0, ec0) kernelData
        in finalPt
        ) points
  in map projectFromPoint transported

inverseTransportCoeffs :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
inverseTransportCoeffs ec steps coeffs =
  let finalCurve = isogenyWalk ec steps
      revSteps   = reverse steps
  in transportCoeffs finalCurve revSteps coeffs

-- ============================================================
-- Dual Isogeny
-- ============================================================

dualIsogenyCurve :: EllipticCurve -> ECPoint -> Int -> EllipticCurve
dualIsogenyCurve ec kernel ell = veluIsogeny ec kernel ell
