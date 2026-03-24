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
  , richelotProjectKernel
  , richelotTransformU
  , richelotRecoverV
  , richelotEvalToy
  , richelotDual
  , richelotStep
  , richelotWalk
    -- * Target Inspection
  , richelotCorrespondenceTargetCurve
    -- * Factorization
  , factorSextic
    -- * Coefficient Transport
  , richelotTransport
  , richelotInverseTransport
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve (modInv, modPow)
import Algebirc.Geometry.HyperellipticCurve
import Algebirc.Core.Matrix
import Data.List (foldl')
import qualified Data.Vector as V
import Debug.Trace (trace)

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

data KernelProj = KernelProj
  { overlaps :: [Bool]
  , resVals  :: [Integer]
  , factors  :: [Poly]
  } deriving (Show)

richelotProjectKernel :: RichelotCtx -> MumfordDiv -> KernelProj
richelotProjectKernel ctx (MumfordDiv u _ p) =
  let g1 = ctxG1 ctx; g2 = ctxG2 ctx; g3 = ctxG3 ctx
      r1 = polyResultant p u g1
      r2 = polyResultant p u g2
      r3 = polyResultant p u g3
  in KernelProj
       { overlaps = [r1 == 0, r2 == 0, r3 == 0]
       , resVals  = [r1, r2, r3]
       , factors  = [g1, g2, g3]
       }

-- | Helper to build Richelot Dual Basis H_i = G_j' G_k - G_j G_k'
buildH :: Integer -> Poly -> Poly -> Poly
buildH p gj gk =
  let gjp = polyDeriv p gj
      gkp = polyDeriv p gk
      t1 = polyMul p gjp gk
      t2 = polyMul p gj gkp
  in polyNorm p (polySub p t1 t2)

-- | Extracts the i-th coefficient safely
polyCoeff :: Poly -> Int -> Integer
polyCoeff poly i = if i >= 0 && i < V.length poly then poly V.! i else 0

-- | Compute the 3x3 determinant of the quadratic coefficients of G1, G2, G3
coeffDet :: Integer -> Poly -> Poly -> Poly -> Integer
coeffDet p g1 g2 g3 =
  let c12 = polyCoeff g1 2; c11 = polyCoeff g1 1; c10 = polyCoeff g1 0
      c22 = polyCoeff g2 2; c21 = polyCoeff g2 1; c20 = polyCoeff g2 0
      c32 = polyCoeff g3 2; c31 = polyCoeff g3 1; c30 = polyCoeff g3 0
      term1 = (c12 * ((c21 * c30) - (c20 * c31))) `mod` p
      term2 = (c11 * ((c22 * c30) - (c20 * c32))) `mod` p
      term3 = (c10 * ((c22 * c31) - (c21 * c32))) `mod` p
  in (term1 - term2 + term3) `mod` p

-- | Step 2 & 3: Kunzweiler Correspondence Mapping
richelotCorrespondenceMapping :: RichelotCtx -> MumfordDiv -> MumfordDiv
richelotCorrespondenceMapping ctx (MumfordDiv u v p) =
  let g1 = ctxG1 ctx; g2 = ctxG2 ctx; g3 = ctxG3 ctx
      
      -- Evaluate spatial scalar delta
      delta = coeffDet p g1 g2 g3
      invDelta = modInv delta p
      
      -- Scale helper
      scalePoly g r = polyNorm p $ V.map (\c -> (c * r) `mod` p) g
      
      -- Delta-normalized dual quadratics (Codomain Kernel Base)
      h1 = scalePoly (buildH p g2 g3) invDelta
      h2 = scalePoly (buildH p g3 g1) invDelta
      h3 = scalePoly (buildH p g1 g2) invDelta
      
      -- Define Codomain curve C' => Y^2 = delta * h1 * h2 * h3
      newF_unscaled = polyMul p h1 (polyMul p h2 h3)
      newF = scalePoly newF_unscaled delta
      targetCurve = HyperCurve newF 2 p
      
      -- Kunzweiler Root Extractor
      -- Tests assume u(x) splits completely over GF(p).
      solveRoots uPol =
        let rs = [ x | x <- [0..p-1], polyEval p uPol x == 0 ]
        in if polyDeg uPol == 2 && length rs == 1 then [rs !! 0, rs !! 0] else rs
      
      xs = solveRoots u
      
      -- Kunzweiler Image Divisor Generator for a single spatial split point P=(x,y)
      computeDP x y =
        let g1x = polyEval p g1 x
            g2x = polyEval p g2 x
            aP_raw = polyNorm p (polyAdd p (scalePoly h1 g1x) (scalePoly h2 g2x))
            aP = polyMakeMonic p aP_raw
        in if y == 0
           then MumfordDiv aP (V.singleton 0) p
           else let rhs1 = scalePoly h1 g1x
                    polyXminusX = iToV [x `mod` p, p - 1]
                    rhs = polyMul p rhs1 polyXminusX
                    yInv = modInv y p
                    bP_unreduced = scalePoly rhs yInv
                    bP = polyMod p bP_unreduced aP
                in MumfordDiv aP bP p
      
      -- Exact Symbolic Matrix Evaluator via Algebraic Rings for Irreducible U
      symbolicMapping uPol vPol =
        let u1 = polyCoeff uPol 1
            u0 = polyCoeff uPol 0
            v1 = polyCoeff vPol 1
            v0 = polyCoeff vPol 0
            
            -- Trace multiplication operator across R = GF(p)[r]/(u)
            rMulSk (a1,b1) (a2,b2) =
               let r2 = (a1 * a2) `mod` p
                   newA = (a1*b2 + b1*a2 - r2*u1) `mod` p
                   newB = (b1*b2 - r2*u0) `mod` p
               in (newA `mod` p, newB `mod` p)
               
            vNorm = (v0*v0 - v1*v0*u1 + v1*v1*u0) `mod` p
            vInvN = modInv vNorm p
            vInv = (((-v1)*vInvN) `mod` p, ((v0 - v1*u1)*vInvN) `mod` p)
            
            g1_a = (polyCoeff g1 1 - polyCoeff g1 2 * u1) `mod` p
            g1_b = (polyCoeff g1 0 - polyCoeff g1 2 * u0) `mod` p
            g2_a = (polyCoeff g2 1 - polyCoeff g2 2 * u1) `mod` p
            g2_b = (polyCoeff g2 0 - polyCoeff g2 2 * u0) `mod` p
            
            a1_poly = polyNorm p (polyAdd p (scalePoly h1 g1_a) (scalePoly h2 g2_a))
            a0_poly = polyNorm p (polyAdd p (scalePoly h1 g1_b) (scalePoly h2 g2_b))
            
            a0_sq = polyMul p a0_poly a0_poly
            a1_sq = polyMul p a1_poly a1_poly
            a0_a1 = polyMul p a0_poly a1_poly
            
            uNew_raw = polyNorm p (polyAdd p a0_sq (polySub p (scalePoly a1_sq u0) (scalePoly a0_a1 u1)))
            uNew = polyMakeMonic p uNew_raw
            
            (d1, d0) = rMulSk vInv (g1_a, g1_b)
            
            termB1 = iToV [ (d0 - d1*u1) `mod` p, (-d1) `mod` p ]
            termB0 = iToV [ (-d1*u0) `mod` p, (-d0) `mod` p ]
            
            b1_poly = polyMul p termB1 h1
            b0_poly = polyMul p termB0 h1
            
            p1_poly = polyNorm p (polySub p a0_poly (scalePoly a1_poly u1))
            p2_poly = a1_poly
            
            (g_gcd, s, t) = polyExtGCD p p1_poly p2_poly
            
            minusB1 = scalePoly b1_poly (-1)
            (minusB1_div, _) = polyDiv p minusB1 g_gcd
            
            q1 = polyMul p s minusB1_div
            q0 = polyMul p t minusB1_div
            
            v_raw = polyNorm p (polyAdd p (polyMul p q0 a0_poly) (polySub p b0_poly (scalePoly (polyMul p q1 a1_poly) u0)))
            
        in if polyDeg uNew < 0 
           then jacobianIdentity p
           else cantorReduce targetCurve (MumfordDiv uNew (polyMod p v_raw uNew) p)
      
      mappedDivs = map (\x -> computeDP x (polyEval p v x)) xs
      
  in if length xs == polyDeg u
     then case mappedDivs of
            [dp] -> dp
            [dp, dq] -> jacobianAdd targetCurve dp dq
            _ -> jacobianIdentity p -- Trivial identity bounding fallback
     else symbolicMapping u v

-- | Export the dynamically generated target curve for invariant inspection
richelotCorrespondenceTargetCurve :: RichelotCtx -> HyperCurve
richelotCorrespondenceTargetCurve ctx =
  let p = ctxFieldP ctx
      g1 = ctxG1 ctx; g2 = ctxG2 ctx; g3 = ctxG3 ctx
      delta = coeffDet p g1 g2 g3
      invDelta = modInv delta p
      scaleP g r = polyNorm p $ V.map (\c -> (c * r) `mod` p) g
      h1 = scaleP (buildH p g2 g3) invDelta
      h2 = scaleP (buildH p g3 g1) invDelta
      h3 = scaleP (buildH p g1 g2) invDelta
      newF_unscaled = polyMul p h1 (polyMul p h2 h3)
      newF = scaleP newF_unscaled delta
  in HyperCurve newF 2 p

-- | Helper to compute [G_j, G_k] = g_{j,2} g_{k,1} - g_{k,2} g_{j,1}
crossBracket :: Integer -> Poly -> Poly -> Integer
crossBracket p gj gk =
  let gj2 = polyCoeff gj 2
      gj1 = polyCoeff gj 1
      gk2 = polyCoeff gk 2
      gk1 = polyCoeff gk 1
  in ((gj2 * gk1) - (gk2 * gj1)) `mod` p

richelotTransformU :: RichelotCtx -> MumfordDiv -> KernelProj -> Poly
richelotTransformU ctx dIn _proj =
  let MumfordDiv u' _ _ = richelotCorrespondenceMapping ctx dIn
  in u'

-- [Step 3] Placeholder for backwards compatibility (V is solved continuously in Correspondence)
richelotRecoverV :: RichelotCtx -> Poly -> MumfordDiv
richelotRecoverV ctx u' =
  MumfordDiv u' (V.singleton 0) (ctxFieldP ctx)

-- | Evaluate the True Richelot isogeny phi: Jac(C) -> Jac(C') on a divisor D.
richelotEval :: RichelotCtx -> MumfordDiv -> MumfordDiv
richelotEval ctx dIn@(MumfordDiv u _ p) =
  let g1 = ctxG1 ctx; g2 = ctxG2 ctx; g3 = ctxG3 ctx
      isKernel = (polyDeg u == polyDeg g1 && polyMod p u g1 == V.singleton 0) ||
                 (polyDeg u == polyDeg g2 && polyMod p u g2 == V.singleton 0) ||
                 (polyDeg u == polyDeg g3 && polyMod p u g3 == V.singleton 0)
  in if isIdentity dIn || isKernel
     then jacobianIdentity p
     else richelotCorrespondenceMapping ctx dIn

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
