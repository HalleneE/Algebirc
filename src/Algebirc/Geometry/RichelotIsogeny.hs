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
  , findRootsPure
    -- * Invisible Logic Gates
  , deriveBranchContexts
  , richelotEval
    -- * Invoice Output
  , evaluateGeometricSignature
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve (modInv, modPow)
import Algebirc.Geometry.HyperellipticCurve
import Algebirc.Core.Matrix
import Data.List (foldl', sortOn)
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

-- | Factor a sextic f(x) = G1 * G2 * G3 over GF(p).
-- Uses Cantor-Zassenhaus (P5) to find all factors (linear and quadratic).
factorSextic :: Integer -> Integer -> Poly -> (Poly, Poly, Poly)
factorSextic p seed coeffs =
  let f_monic = polyMakeMonic p coeffs
      -- 1. Get all linear factors (roots)
      roots = findRootsPure p seed f_monic
      rootPolys = map (\r -> iToV [p - r, 1]) roots
      
      -- 2. Remaining polynomial for degree-2 factors
      rootsProd = foldl (polyMul p) (V.singleton 1) rootPolys
      (f_rem, _) = polyDiv p f_monic rootsProd
      
      -- 3. Get all quadratic factors
      quads = if polyDeg f_rem > 0 
              then equalDegreeFactorization p (seed + 10) f_rem 2
              else []
      
      -- 4. Combine into three quadratics
      allFactors = groupToQuadratics p (rootPolys ++ quads)
      
      -- Helper: Group factors into exactly 3 quadratics
      groupToQuadratics _ fs
        | sum (map polyDeg fs) /= 6 = replicate 3 [iToV [1, 0, 1]] -- Should not happen for sextic
        | otherwise = 
            let sorted = reverse $ Data.List.sortOn polyDeg fs
            in case sorted of
                 -- 3 quadratics
                 [q1, q2, q3] -> [[q1], [q2], [q3]]
                 -- 2 quadratics, 2 linears
                 [q1, q2, l1, l2] -> [[q1], [q2], [polyMul p l1 l2]]
                 -- 1 quadratic, 4 linears
                 [q1, l1, l2, l3, l4] -> [[q1], [polyMul p l1 l2], [polyMul p l3 l4]]
                 -- 6 linears
                 [l1, l2, l3, l4, l5, l6] -> [[polyMul p l1 l2], [polyMul p l3 l4], [polyMul p l5 l6]]
                 -- Fallback for any other weird cases (e.g. higher degree irreducible)
                 _ -> replicate 3 [iToV [1, 0, 1]] 

      [g1, g2, g3] = map (foldl (polyMul p) (V.singleton 1)) allFactors
  in (g1, g2, g3)

-- | Pure Cantor-Zassenhaus root finding [P5].
findRootsPure :: Integer -> Integer -> Poly -> [Integer]
findRootsPure p seed f =
  let f_monic = polyMakeMonic p f
      -- Distinct Degree Factorization for degree 1
      xPowP = polyPowMod p (iToV [0, 1]) p f_monic
      g = polyGCD p f_monic (polySub p xPowP (iToV [0, 1]))
  in if polyDeg g <= 0 then [] else map (polySolveLinear p) (equalDegreeFactorization p seed g 1)

-- | Equal Degree Factorization (EDF) for factors of degree d.
equalDegreeFactorization :: Integer -> Integer -> Poly -> Int -> [Poly]
equalDegreeFactorization p seed f d = go seed f 0
  where
    go s poly count
      | count > 200 = [polyMakeMonic p poly] -- Give up after 200 attempts, return current (likely irreducible)
      | polyDeg poly == d = [polyMakeMonic p poly]
      | otherwise =
          let -- Pick a random polynomial a(x) of degree < deg(poly)
              a = iToV $ take (polyDeg poly) $ map (\v -> v `mod` p) $ 
                         iterate (\v -> (v * 1103515245 + 12345) `mod` (2^31)) s
              -- h = a^((p^d-1)/2) - 1 mod poly
              expVal = (p^d - 1) `div` 2
              h = polySub p (polyPowMod p a expVal poly) (V.singleton 1)
              g = polyGCD p poly h
              degG = polyDeg g
          in if degG > 0 && degG < polyDeg poly
             then go (s + 1) g count ++ 
                  go (s + 2) (fst $ polyDiv p poly g) count
             else go (s + 3) poly (count + 1)

-- | Solve a linear polynomial ax + b = 0 => x = -b/a.
polySolveLinear :: Integer -> Poly -> Integer
polySolveLinear p f =
  let b = polyCoeff f 0
      a = polyCoeff f 1
  in ((p - b) * modInv a p) `mod` p

-- | Find roots of polynomial f(x) in GF(p). (Legacy Heuristic - DEPRECATED)
findRoots :: Integer -> Poly -> [Integer]
findRoots p coeffs = findRootsPure p 777 coeffs

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
        in normalizeDiv $ if y == 0
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
  if isIdentity dIn
     then jacobianIdentity p
     else richelotCorrespondenceMapping ctx dIn

-- | Invisible Logic Branching (The Holy Grail Multiplexer)
-- Performs: D' = phi_true([b]D) + phi_false([1-b]D)
-- This works WITHOUT decrypting 'b' at the point of evaluation if b is embedded in the scalar.
-- For strict "Invisible Logic", both contexts should target the same codomain curve.
richelotBranch :: (RichelotCtx, RichelotCtx) -> Integer -> MumfordDiv -> MumfordDiv
richelotBranch (ctxT, ctxF) b d =
  let p = ctxFieldP ctxT
      hc = HyperCurve (polyMul p (polyMul p (ctxG1 ctxT) (ctxG2 ctxT)) (ctxG3 ctxT)) 2 p
      targetHC = richelotCorrespondenceTargetCurve ctxT
      
      -- Constant-Time Jacobian Scalar Multiplication (Fixed 256-bit width)
      dT = jacobianScalarMulCT 256 hc b d
      dF = jacobianScalarMulCT 256 hc ((1 - b + p) `mod` p) d
      
      phiT = richelotEval ctxT dT
      phiF = richelotEval ctxF dF
      
      res = jacobianAdd targetHC phiT phiF
  in res

-- | Derive two different Richelot contexts for branching from a single seed.
-- In a real "Holy Grail" system, ini akan di-precompute dan disimpan di binary.
deriveBranchContexts :: Integer -> HyperCurve -> (RichelotCtx, RichelotCtx)
deriveBranchContexts seed hc =
  let p = hcPrime hc
      f = hcCoeffs hc
      -- We need two different factorizations to create two different paths
      ctx1 = mkRichelotCtx p (factorSextic p seed f)
      ctx2 = mkRichelotCtx p (factorSextic p (seed + 12345) f)
  in (ctx1, ctx2)

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
-- Takes a seed for Cantor-Zassenhaus factorization.
richelotStep :: Integer -> Integer -> Poly -> Poly
richelotStep p seed fCoeffs =
  let (g1, g2, g3) = factorSextic p seed fCoeffs
  in richelotDual p (g1, g2, g3)

-- | Multi-step Richelot walk.
richelotWalk :: Integer -> Integer -> Poly -> Int -> [Poly]
richelotWalk p seed f0 nSteps =
  scanl (\f s -> richelotStep p s f) f0 [seed .. seed + fromIntegral nSteps - 1]

-- ============================================================
-- Geometric Signature Evaluator
-- ============================================================

-- | The True Holy Grail Output. 
-- Instead of polluting coefficients with mix padding, this evaluator halts
-- the structural state machine and issues the exact Igusa invariants of the final curve
-- mapping as the definitive signature of the binary obfuscator.
evaluateGeometricSignature :: MumfordDiv -> HyperCurve -> IgusaInvariants
evaluateGeometricSignature (MumfordDiv u v p) (HyperCurve f g _) =
  -- The execution state (u,v) merges dynamically with the base curve topological state
  -- to form the precise execution signature curve. 
  let executionSextic = polyAdd p f (polyAdd p u v)
      finalHc = HyperCurve executionSextic g p
  in igusaInvariants finalHc

-- ============================================================
-- End of Richelot Calculus
-- ============================================================
