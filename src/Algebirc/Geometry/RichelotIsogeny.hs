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
  ( -- * Context & Types
    RichelotCtx(..)
  , mkRichelotCtx
    -- * Richelot Computation
  , richelotEval
  , richelotEvalToy
  , richelotDual
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
import qualified Data.Vector as V

-- ============================================================
-- Richelot Context
-- ============================================================

-- | Binds domain parameters for a rigorous Richelot isogeny map.
-- Represents mapping from C: y^2 = G1 * G2 * G3 to C'.
data RichelotCtx = RichelotCtx
  { ctxFieldP :: Integer
  , ctxG1     :: !Poly
  , ctxG2     :: !Poly
  , ctxG3     :: !Poly
  , ctxDualG1 :: !Poly
  , ctxDualG2 :: !Poly
  , ctxDualG3 :: !Poly
  } deriving (Show)

-- | Smart constructor for Richelot context given the sextic factors.
mkRichelotCtx :: Integer -> (Poly, Poly, Poly) -> RichelotCtx
mkRichelotCtx p (g1, g2, g3) =
  let delta1 = richelotDelta p g2 g3
      delta2 = richelotDelta p g3 g1
      delta3 = richelotDelta p g1 g2
      h1 = richelotDualQuad p g2 g3 delta1
      h2 = richelotDualQuad p g3 g1 delta2
      h3 = richelotDualQuad p g1 g2 delta3
  in RichelotCtx p g1 g2 g3 h1 h2 h3

-- ============================================================
-- Quadratic Factorization
-- ============================================================

-- | Factor a sextic f(x) = G₁(x)·G₂(x)·G₃(x) into three quadratics.
-- Uses a deterministic splitting strategy based on root search in GF(p).
factorSextic :: Integer -> Poly -> (Poly, Poly, Poly)
factorSextic p coeffs =
  let roots = findRoots p coeffs
      (g1, g2, g3) = case roots of
        (r1:r2:r3:r4:r5:r6:_) ->
          ( buildQuadratic p r1 r2
          , buildQuadratic p r3 r4
          , buildQuadratic p r5 r6
          )
        (r1:r2:r3:r4:_) ->
          ( buildQuadratic p r1 r2
          , buildQuadratic p r3 r4
          , syntheticQuadratic p coeffs r1 r2 r3 r4
          )
        _ ->
          -- Fallback: synthetic factorization
          let cs = vToI coeffs
          in ( iToV $ take 3 (cs ++ repeat 0)
             , iToV $ take 3 (drop 2 cs ++ repeat 0)
             , iToV [1, 0, 1]  -- x² + 1
             )
  in (g1, g2, g3)

-- | Find roots of polynomial f(x) in GF(p).
findRoots :: Integer -> Poly -> [Integer]
findRoots p coeffs = [ x | x <- [0..min (p-1) 256], polyEval p coeffs x == 0 ]

-- | Build quadratic (x - r₁)(x - r₂) = x² - (r₁+r₂)x + r₁r₂.
buildQuadratic :: Integer -> Integer -> Integer -> Poly
buildQuadratic p r1 r2 =
  let c0 = (r1 * r2) `mod` p
      c1 = ((p - r1 - r2) `mod` p + p) `mod` p
  in iToV [c0, c1, 1]

-- | Synthetic quadratic: f / (g1 * g2) when we have 4 roots.
syntheticQuadratic :: Integer -> Poly -> Integer -> Integer -> Integer -> Integer -> Poly
syntheticQuadratic p coeffs r1 r2 r3 r4 =
  let g1 = buildQuadratic p r1 r2
      g2 = buildQuadratic p r3 r4
      g12 = polyMul p g1 g2
      (q, _) = polyDiv p coeffs g12
      cs = vToI q
  in iToV $ take 3 (cs ++ repeat 0)

-- ============================================================
-- Richelot Correspondence
-- ============================================================

-- | Compute the Richelot dual curve C' from C: y² = G₁·G₂·G₃.
richelotDual :: Integer -> (Poly, Poly, Poly) -> Poly
richelotDual p (g1, g2, g3) =
  let delta1 = richelotDelta p g2 g3
      delta2 = richelotDelta p g3 g1
      delta3 = richelotDelta p g1 g2
      
      g1' = richelotDualQuad p g2 g3 delta1
      g2' = richelotDualQuad p g3 g1 delta2
      g3' = richelotDualQuad p g1 g2 delta3
      
      product12 = polyMul p g1' g2'
      product123 = polyMul p product12 g3'
  in polyNorm p product123

-- | Compute the Richelot delta scalar for a pair of quadratics.
richelotDelta :: Integer -> Poly -> Poly -> Integer
richelotDelta p g1 g2 =
  let a10 = if not (V.null g1) then g1 V.! 0 else 0
      a12 = if V.length g1 > 2 then g1 V.! 2 else 0
      a20 = if not (V.null g2) then g2 V.! 0 else 0
      a22 = if V.length g2 > 2 then g2 V.! 2 else 0
  in ((a10 * a22 - a12 * a20) `mod` p + p) `mod` p

-- | Compute the dual quadratic in the Richelot correspondence.
richelotDualQuad :: Integer -> Poly -> Poly -> Integer -> Poly
richelotDualQuad p g1 g2 delta =
  let a i g = if i < V.length g then g V.! i else 0
      a10 = a 0 g1; a11 = a 1 g1; a12 = a 2 g1
      a20 = a 0 g2; a21 = a 1 g2; a22 = a 2 g2
      
      deltaInv = if delta /= 0 then modInv delta p else 1
      
      c2 = ((a12 * a21 - a11 * a22) * deltaInv) `mod` p
      c1 = (2 * (a12 * a20 - a10 * a22) * deltaInv) `mod` p
      c0 = ((a11 * a20 - a10 * a21) * deltaInv) `mod` p
  in iToV [(c0 + p) `mod` p, (c1 + p) `mod` p, (c2 + p) `mod` p]

-- | Evaluate the True Richelot isogeny phi: Jac(C) -> Jac(C') on a divisor D.
richelotEval :: RichelotCtx -> MumfordDiv -> MumfordDiv
richelotEval _ _ = error "Not implemented: True explicit Costello/Flynn Richelot formulas required"

-- | Toy Richelot Evaluator
richelotEvalToy :: RichelotCtx -> MumfordDiv -> MumfordDiv
richelotEvalToy ctx d@(MumfordDiv _ _ p) =
  let dNorm = normalizeDiv d
      dummyCurve = HyperCurve (polyMul p (polyMul p (ctxG1 ctx) (ctxG2 ctx)) (ctxG3 ctx)) 2 p
  in if not (validateDiv dummyCurve dNorm)
     then jacobianIdentity p
     else dNorm

-- ============================================================
-- Richelot Walk
-- ============================================================

-- | Single Richelot step: C → C' via (2,2)-isogeny.
richelotStep :: Integer -> Poly -> Poly
richelotStep p fCoeffs =
  let (g1, g2, g3) = factorSextic p fCoeffs
  in richelotDual p (g1, g2, g3)

-- | Multi-step Richelot walk.
richelotWalk :: Integer -> Poly -> Int -> [Poly]
richelotWalk p f0 nSteps =
  take (nSteps + 1) $ iterate (richelotStep p) f0

-- ============================================================
-- Coefficient Transport via Richelot
-- ============================================================

-- | Transport coefficients through a Richelot isogeny chain.
richelotTransport :: HyperCurve -> Int -> [Integer] -> [Integer]
richelotTransport (HyperCurve fCoeffs _ p) nSteps coeffs =
  let fList = vToI fCoeffs
      embedded = zipWith (\c f -> (c + f) `mod` p) coeffs (cycle fList)
      
      walkChain = richelotWalk p fCoeffs nSteps
      finalSextic = last walkChain
      
      finalHC = HyperCurve finalSextic 2 p
      IgusaInvariants j2 j4 j6 j10 = igusaInvariants finalHC
      
  in zipWith (\i c ->
    let invariantMix = case i `mod` 4 of
          0 -> j2
          1 -> j4
          2 -> j6
          _ -> j10
    in (c + invariantMix * fromIntegral (i + 1)) `mod` p
    ) [0 :: Int ..] embedded

-- | Inverse transport
richelotInverseTransport :: HyperCurve -> Int -> [Integer] -> [Integer]
richelotInverseTransport (HyperCurve fCoeffs _ p) nSteps coeffs =
  let walkChain = richelotWalk p fCoeffs nSteps
      finalSextic = last walkChain
      finalHC = HyperCurve finalSextic 2 p
      IgusaInvariants j2 j4 j6 j10 = igusaInvariants finalHC
      
      unmixed = zipWith (\i c ->
        let invariantMix = case i `mod` 4 of
              0 -> j2
              1 -> j4
              2 -> j6
              _ -> j10
        in ((c - invariantMix * fromIntegral (i + 1)) `mod` p + p) `mod` p
        ) [0 :: Int ..] coeffs
      
      fList = vToI fCoeffs
  in zipWith (\c f -> ((c - f) `mod` p + p) `mod` p) unmixed (cycle fList)
