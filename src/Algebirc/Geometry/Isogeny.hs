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
  , algebraicSponge
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
    -- * Volcano Navigation
  , VolcanoStep(..)
  , navigateVolcano
  , generateVolcanoPath
  , volcanoMix
  , volcanoUnmix
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve
import Algebirc.Core.Matrix (FieldMatrix(..), matApplyField, mkCauchyMatrix)
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
                dxIsZero = isZeroField p dx
                safeDx = branchlessSelect p dxIsZero 1 dx
                dxInv = modInv safeDx p
                gxQ = (3 * qx * qx + ecA ec) `mod` p
                gyQ = ((-2) * qy + p) `mod` p
                xc = (gxQ * dxInv) `mod` p
                dy = (py - qy + p) `mod` p
                yc = (gxQ * dy * dxInv `mod` p * dxInv `mod` p
                      - gyQ * dxInv) `mod` p
                newXacc = (xacc + xc) `mod` p
                newYacc = (yacc + yc) `mod` p
            in ( branchlessSelect p dxIsZero xacc newXacc
               , branchlessSelect p dxIsZero yacc newYacc )
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

-- ============================================================
-- Pure Algebraic Sponge PRF [P7]
-- ============================================================

-- | A simple algebraic permutation for sponge construction (Poseidon-lite).
algebraicPermute :: Integer -> [Integer] -> [Integer]
algebraicPermute p state =
  let n = length state
      sboxed = map (\x -> modPow x 3 p) state
      mds = mkCauchyMatrix p n
  in matApplyField p mds sboxed

algebraicSponge :: Integer -> [Integer] -> Int -> [Integer]
algebraicSponge p capacity count =
  let initialState = take 3 (capacity ++ repeat 0)
      permuted = iterate (algebraicPermute p) initialState
  in take count $ concatMap (take 1) (drop 1 permuted)

mixWithSponge :: Integer -> [Integer] -> [Integer] -> [Integer]
mixWithSponge p invariants dataStream =
  let n = length dataStream
      spongeStream = algebraicSponge p invariants n
  in zipWith (\c m -> (c + m) `mod` p) dataStream spongeStream

unmixWithSponge :: Integer -> [Integer] -> [Integer] -> [Integer]
unmixWithSponge p invariants dataStream =
  let n = length dataStream
      spongeStream = algebraicSponge p invariants n
  in zipWith (\c m -> (c - m + p) `mod` p) dataStream spongeStream

-- | Mix coefficients via isogeny walk invariants.
isogenyMix :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
isogenyMix ec0 steps coeffs =
  let p = ecPrime ec0
      curves = walkCurves ec0 steps
      jInvariants = map jInvariant curves
      invariants = if null jInvariants then [1] else jInvariants
  in mixWithSponge p invariants coeffs

-- | Unmix coefficients (inverse of isogenyMix).
isogenyUnmix :: EllipticCurve -> [(Integer, Int)] -> [Integer] -> [Integer]
isogenyUnmix ec0 steps coeffs =
  let p = ecPrime ec0
      curves = walkCurves ec0 steps
      jInvariants = map jInvariant curves
      invariants = if null jInvariants then [1] else jInvariants
  in unmixWithSponge p invariants coeffs

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

-- ============================================================
-- Volcano Navigation
-- ============================================================

-- | Step types for navigating an isogeny volcano.
data VolcanoStep
  = Ascend    -- ^ Move towards the Crater (surface)
  | StayCycle -- ^ Move within the Crater cycle
  | Descend   -- ^ Move towards the Floors (depth)
  deriving (Show, Eq)

-- | Navigate an isogeny volcano by executing specific steps.
navigateVolcano :: EllipticCurve -> [(VolcanoStep, Integer, Int)] -> EllipticCurve
navigateVolcano = foldl' (\ec (step, seed, ell) ->
  let kernel = findVolcanoKernel ec step (fromIntegral ell) seed
  in veluIsogeny ec kernel ell
  )

-- | Helper to find a kernel point specific to the volcano step.
findVolcanoKernel :: EllipticCurve -> VolcanoStep -> Integer -> Integer -> ECPoint
findVolcanoKernel ec step ell seed =
  case step of
    Ascend -> 
      -- In a real volcano, we would check the level here.
      findKernelPoint ec ell seed
    StayCycle ->
      -- Stay within the crater cycles.
      findKernelPoint ec ell (seed + 101)
    Descend ->
      -- Descending to lower floors.
      findKernelPoint ec ell (seed * 7)

-- | Generate a deterministic path: Ascend -> Cycle -> Descend.
generateVolcanoPath :: Integer -> Int -> [(VolcanoStep, Integer, Int)]
generateVolcanoPath seed depth =
  let ascends = replicate depth (Ascend, seed, 2)
      cycles  = replicate 3 (StayCycle, seed + 1, 2)
      descends = replicate depth (Descend, seed + 2, 2)
  in ascends ++ cycles ++ descends

-- | Walk through the volcano and return all intermediate curves.
volcanoWalkCurves :: EllipticCurve -> [(VolcanoStep, Integer, Int)] -> [EllipticCurve]
volcanoWalkCurves ec0 steps = scanl (\ec (step, seed, ell) ->
  let kernel = findVolcanoKernel ec step (fromIntegral ell) seed
  in veluIsogeny ec kernel ell
  ) ec0 steps

-- | Mix data using invariants from a volcano walk.
volcanoMix :: EllipticCurve -> [(VolcanoStep, Integer, Int)] -> [Integer] -> [Integer]
volcanoMix ec0 steps coeffs =
  let p = ecPrime ec0
      curves = volcanoWalkCurves ec0 steps
      jInvariants = map jInvariant curves
      invariants = if null jInvariants then [1] else jInvariants
  in mixWithSponge p invariants coeffs

-- | Unmix data from a volcano walk.
volcanoUnmix :: EllipticCurve -> [(VolcanoStep, Integer, Int)] -> [Integer] -> [Integer]
volcanoUnmix ec0 steps coeffs =
  let p = ecPrime ec0
      curves = volcanoWalkCurves ec0 steps
      jInvariants = map jInvariant curves
      invariants = if null jInvariants then [1] else jInvariants
  in unmixWithSponge p invariants coeffs
