-- |
-- Module      : Algebirc.Geometry.HyperellipticCurve
-- Description : Genus-2 hyperelliptic curves, Jacobian arithmetic, Igusa invariants
-- License     : MIT
--
-- = Mathematical Foundation (Algebra Engine V2)
--
-- A genus-2 hyperelliptic curve: C: y² = f(x) where deg(f) ∈ {5, 6} over GF(p).
-- Memory Layout: Boxed Vectors (Integer) for GF(p) polynomials.
-- This ensures safety for 256-bit cryptographic primes without silent integer truncation.

module Algebirc.Geometry.HyperellipticCurve
  ( -- * Curve Construction
    mkHyperCurve
  , hyperDiscriminant
    -- * Polynomial Arithmetic (in GF(p))
  , polyAdd
  , polySub
  , polyMul
  , polyMulNaive
  , polyMulKaratsuba
  , polyMod
  , polyDiv
  , polyEval
  , polyLeadCoeff
  , polyDeg
  , polyNorm
  , polyExtGCD
  , polyMakeMonic
  , polyDeriv
  , iToV
  , vToI
    -- * Jacobian Arithmetic (Cantor)
  , jacobianAdd
  , jacobianDouble
  , jacobianNegate
  , jacobianScalarMul
  , jacobianIdentity
  , isIdentity
  , cantorReduce
  , cantorCompose
    -- * Divisor Invariants
  , validateDiv
  , normalizeDiv
    -- * Igusa Invariants
  , igusaInvariants
  , igusaClebsch
    -- * Coefficient Routing
  , coeffsToJacobian
  , jacobianToCoeffs
  , mixViaJacobian
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve (modInv, modPow)
import Data.List (foldl')
import qualified Data.Vector as V
import Debug.Trace (traceStack)

-- ============================================================
-- Vector Interop Helpers
-- ============================================================

vToI :: Poly -> [Integer]
vToI = V.toList

iToV :: [Integer] -> Poly
iToV = V.fromList

-- ============================================================
-- Polynomial Arithmetic in GF(p)[x] (Vector Native)
-- ============================================================

-- | Normalize: remove trailing zeros, reduce mod p.
polyNorm :: Integer -> Poly -> Poly
polyNorm p v =
  let reduced = V.map (`mod` p) v
      reversed = V.dropWhile (== 0) (V.reverse reduced)
  in if V.null reversed then V.singleton 0 else V.reverse reversed

-- | Polynomial degree (-1 for zero polynomial).
polyDeg :: Poly -> Int
polyDeg v =
  if V.length v <= 1 && (V.null v || v V.! 0 == 0)
  then -1 else V.length v - 1

-- | Leading coefficient.
polyLeadCoeff :: Poly -> Integer
polyLeadCoeff v = if V.null v || polyDeg v == -1 then 0 else V.last v

-- | Polynomial addition.
polyAdd :: Integer -> Poly -> Poly -> Poly
polyAdd p as bs =
  let n = max (V.length as) (V.length bs)
      as' = as V.++ V.replicate (n - V.length as) 0
      bs' = bs V.++ V.replicate (n - V.length bs) 0
  in polyNorm p $ V.zipWith (\a b -> (a + b) `mod` p) as' bs'

-- | Polynomial subtraction. (a - b) mod p
polySub :: Integer -> Poly -> Poly -> Poly
polySub p as bs =
  let n = max (V.length as) (V.length bs)
      as' = as V.++ V.replicate (n - V.length as) 0
      bs' = bs V.++ V.replicate (n - V.length bs) 0
  in polyNorm p $ V.zipWith (\a b -> ((a - b) `mod` p + p) `mod` p) as' bs'

-- | Polynomial multiplication (Hybrid: Naive for small, Karatsuba for large).
polyMul :: Integer -> Poly -> Poly -> Poly
polyMul p as bs
  | V.null as || V.null bs = V.singleton 0
  | polyDeg as == -1 || polyDeg bs == -1 = V.singleton 0
  | V.length as < 16 || V.length bs < 16 = polyMulNaive p as bs
  | otherwise = polyMulKaratsuba p as bs

-- | Naive O(n^2) Polynomial Multiplication.
polyMulNaive :: Integer -> Poly -> Poly -> Poly
polyMulNaive p as bs =
      let na = V.length as
          nb = V.length bs
          n = na + nb - 1
          result = V.generate n $ \k ->
            let start = max 0 (k - nb + 1)
                end   = min k (na - 1)
                sumVal = V.foldl' (\acc i ->
                   (acc + (as V.! i) * (bs V.! (k - i))) `mod` p
                 ) 0 (V.enumFromN start (end - start + 1))
            in sumVal `mod` p
      in polyNorm p result

-- | Karatsuba O(n^1.58) Polynomial Multiplication natively on Vectors.
polyMulKaratsuba :: Integer -> Poly -> Poly -> Poly
polyMulKaratsuba p as bs =
  let len = max (V.length as) (V.length bs)
      m = len `div` 2
      
      (a0, a1) = if V.length as <= m then (as, V.singleton 0) else V.splitAt m as
      (b0, b1) = if V.length bs <= m then (bs, V.singleton 0) else V.splitAt m bs

      z0 = polyMul p a0 b0
      z2 = polyMul p a1 b1
      
      aSum = polyAdd p a0 a1
      bSum = polyAdd p b0 b1
      
      z1 = polySub p (polySub p (polyMul p aSum bSum) z0) z2
      
      shift poly k = if polyDeg poly == -1 then V.singleton 0 else V.replicate k 0 V.++ poly
  in polyNorm p $ polyAdd p (polyAdd p (shift z2 (2 * m)) (shift z1 m)) z0

-- | Polynomial division with remainder.
polyDiv :: Integer -> Poly -> Poly -> (Poly, Poly)
polyDiv p num den
  | polyDeg den < 0 = (V.singleton 999, V.singleton 888)
  | polyDeg num < polyDeg den = (V.singleton 0, num)
  | otherwise =
      let lc = polyLeadCoeff den
          lcInv = modInv lc p
          degDiff = polyDeg num - polyDeg den
      in goDiv (polyNorm p num) den lcInv p degDiff (V.replicate (degDiff + 1) 0)

goDiv :: Poly -> Poly -> Integer -> Integer -> Int -> Poly -> (Poly, Poly)
goDiv rem' den lcInv p degDiff quot
  | polyDeg rem' < polyDeg den = (polyNorm p quot, polyNorm p rem')
  | otherwise =
      let dd = polyDeg rem' - polyDeg den
          coeff = (polyLeadCoeff rem' * lcInv) `mod` p
          mono = V.replicate dd 0 V.++ V.singleton coeff
          subtracted = polyMul p mono den
          newRem = polySub p rem' subtracted
          newQuot = polyAdd p quot mono
      in goDiv newRem den lcInv p degDiff newQuot

-- | Polynomial modular remainder.
polyMod :: Integer -> Poly -> Poly -> Poly
polyMod p num den = snd (polyDiv p num den)

-- | Extended GCD.
polyExtGCD :: Integer -> Poly -> Poly -> (Poly, Poly, Poly)
polyExtGCD p a b
  | polyDeg b < 0 =
      if polyDeg a < 0
      then (V.singleton 0, V.singleton 0, V.singleton 0)
      else
        let lc = polyLeadCoeff a
            lcInv = modInv lc p
        in (polyMakeMonic p a, V.singleton lcInv, V.singleton 0)
  | otherwise =
      let (q, r) = polyDiv p a b
          (g, s, t) = polyExtGCD p b r
      in (g, t, polySub p s (polyMul p q t))

-- | Evaluate polynomial at a point.
polyEval :: Integer -> Poly -> Integer -> Integer
polyEval p coeffs x =
  V.foldl' (\acc (i, c) ->
    (acc + c * modPow x (fromIntegral i) p) `mod` p
  ) 0 (V.indexed coeffs)

-- | Make polynomial monic.
polyMakeMonic :: Integer -> Poly -> Poly
polyMakeMonic _ v | polyDeg v < 0 = V.singleton 0
polyMakeMonic p xs =
  let lc = polyLeadCoeff xs
      lcInv = modInv lc p
  in polyNorm p $ V.map (\c -> (c * lcInv) `mod` p) xs

-- ============================================================
-- Curve Construction
-- ============================================================

mkHyperCurve :: [Integer] -> Integer -> Either String HyperCurve
mkHyperCurve coeffs p
  | p <= 2 = Left "Prime must be > 2"
  | deg < 5 || deg > 6 = Left $ "Genus-2 requires deg(f) ∈ {5,6}, got " ++ show deg
  | otherwise = Right $ HyperCurve (iToV $ map (`mod` p) coeffs) 2 p
  where
    deg = length (dropWhile (== 0) (reverse coeffs)) - 1

hyperDiscriminant :: HyperCurve -> Integer
hyperDiscriminant (HyperCurve coeffs _ p) =
  let f' = polyDeriv p coeffs
      g = polyExtGCD p coeffs f'
      (gc, _, _) = g
  in if polyDeg gc > 0 then 0 else polyLeadCoeff coeffs `mod` p

polyDeriv :: Integer -> Poly -> Poly
polyDeriv p coeffs
  | V.length coeffs <= 1 = V.singleton 0
  | otherwise = polyNorm p $ V.generate (V.length coeffs - 1) $ \i ->
      let c = coeffs V.! (i + 1)
          idx = fromIntegral (i + 1)
      in (c * idx) `mod` p

-- ============================================================
-- Jacobian Arithmetic (Cantor's Algorithm)
-- ============================================================

jacobianIdentity :: Integer -> MumfordDiv
jacobianIdentity p = MumfordDiv (V.singleton 1) (V.singleton 0) p

jacobianNegate :: HyperCurve -> MumfordDiv -> MumfordDiv
jacobianNegate (HyperCurve _ _ p) (MumfordDiv u v _) =
  let negV = polyNorm p $ V.map (\c -> (p - c) `mod` p) v
  in MumfordDiv u negV p

jacobianAdd :: HyperCurve -> MumfordDiv -> MumfordDiv -> MumfordDiv
jacobianAdd hc d1 d2
  | isIdentity d1 = d2
  | isIdentity d2 = d1
  | otherwise = cantorCompose hc d1 d2

isIdentity :: MumfordDiv -> Bool
isIdentity (MumfordDiv u _ _) = polyDeg u <= 0 && (if V.null u then False else u V.! 0 == 1)

cantorCompose :: HyperCurve -> MumfordDiv -> MumfordDiv -> MumfordDiv
cantorCompose (HyperCurve fCoeffs g p) (MumfordDiv u1 v1 _) (MumfordDiv u2 v2 _) =
  let (d1, e1, e2) = polyExtGCD p u1 u2
      vSum = polyAdd p v1 v2
      (d, c1, c2) = polyExtGCD p d1 vSum
      s1 = polyMul p c1 e1
      s2 = polyMul p c1 e2
      s3 = c2
      u1u2 = polyMul p u1 u2
      d2 = polyMul p d d
      (uNew, _) = polyDiv p u1u2 (if polyDeg d2 > 0 then d2 else V.singleton 1)
      t1 = polyMul p (polyMul p s1 u1) v2
      t2 = polyMul p (polyMul p s2 u2) v1
      v1v2 = polyMul p v1 v2
      t3inner = polyAdd p v1v2 fCoeffs
      t3' = if V.null s3 then V.singleton 0 else polyMul p s3 t3inner
      vNewNum = polyAdd p (polyAdd p t1 t2) t3'
      (vNew, _) = if polyDeg d > 0 then polyDiv p vNewNum d else (vNewNum, V.singleton 0)
      vReduced = polyMod p vNew uNew
  in cantorReduce (HyperCurve fCoeffs g p) (MumfordDiv (polyMakeMonic p uNew) (polyNorm p vReduced) p)

cantorReduce :: HyperCurve -> MumfordDiv -> MumfordDiv
cantorReduce hc@(HyperCurve fCoeffs g p) (MumfordDiv u v _)
  | polyDeg u <= g = MumfordDiv (polyMakeMonic p u) v p
  | otherwise =
      let degF = polyDeg fCoeffs
          isEvenDegree = degF `mod` 2 == 0
          
          vNorm = polyMod p v u
          degU = polyDeg u
          
          -- Mathematical Parity Modification for Even Degree Sextics
          -- When trying to reduce degree 3 -> 2 over sextics, we MUST match the curve's infinity rational asymptotes 
          -- by interpolating V(X) such that leading coefficient of V(x)^2 zeroes the leading coefficient of f(x).
          vAdj = if isEvenDegree && degU == g + 1
                 then let fLc = polyLeadCoeff fCoeffs
                          uLc = polyLeadCoeff u
                          qSq = (fLc * modInv (uLc * uLc `mod` p) p) `mod` p
                          roots = [ y | y <- [0..p-1], (y*y) `mod` p == qSq ]
                      in if null roots
                         then vNorm -- The divisor CANNOT be represented minimally across GF(p) (no rational points at infinity)
                         else let qPoly = V.singleton (head roots)
                              in polyAdd p vNorm (polyMul p qPoly u)
                 else vNorm

          v2 = polyMul p vAdj vAdj
          fMinusV2 = polySub p fCoeffs v2
          (uNew, _) = polyDiv p fMinusV2 u
          
      in if polyDeg uNew < 0 
         then jacobianIdentity p
         else let negV = polyNorm p $ V.map (\c -> (p - c) `mod` p) vAdj
                  vNew = polyMod p negV uNew
              in if polyDeg uNew >= polyDeg u
                 then MumfordDiv (polyMakeMonic p uNew) (polyNorm p vNew) p
                 else cantorReduce hc (MumfordDiv (polyMakeMonic p uNew) (polyNorm p vNew) p)

-- ============================================================
-- Divisor Invariants & Validation
-- ============================================================

normalizeDiv :: MumfordDiv -> MumfordDiv
normalizeDiv (MumfordDiv u v p) =
  let uMonic = polyMakeMonic p u
      vReduced = polyMod p v uMonic
  in MumfordDiv uMonic (polyNorm p vReduced) p

validateDiv :: HyperCurve -> MumfordDiv -> Bool
validateDiv (HyperCurve fCoeffs g p) (MumfordDiv u v _) =
  let uNorm = polyNorm p u
      vNorm = polyNorm p v
      isMonic = if V.null uNorm then False else (V.last uNorm == 1)
      degU = polyDeg uNorm
      degV = polyDeg vNorm
      degCheck = if degU == 0 then (polyDeg vNorm < 0) else (degV < degU)
      v2 = polyMul p vNorm vNorm
      v2MinusF = polySub p v2 fCoeffs
      (_, remData) = polyDiv p v2MinusF uNorm
      divides = polyDeg (polyNorm p remData) < 0
  in isMonic && (degU <= g) && degCheck && divides

jacobianDouble :: HyperCurve -> MumfordDiv -> MumfordDiv
jacobianDouble hc d = jacobianAdd hc d d

jacobianScalarMul :: HyperCurve -> Integer -> MumfordDiv -> MumfordDiv
jacobianScalarMul hc n d
  | n == 0    = jacobianIdentity (hcPrime hc)
  | n == 1    = d
  | n < 0     = jacobianScalarMul hc (-n) (jacobianNegate hc d)
  | even n    = let half = jacobianScalarMul hc (n `div` 2) d
                in jacobianDouble hc half
  | otherwise = jacobianAdd hc d (jacobianScalarMul hc (n - 1) d)

-- ============================================================
-- Igusa Invariants
-- ============================================================

igusaInvariants :: HyperCurve -> IgusaInvariants
igusaInvariants (HyperCurve coeffs _ p) =
  let c i = if i < V.length coeffs then coeffs V.! i else 0
      f0 = c 0; f1 = c 1; f2 = c 2; f3 = c 3; f4 = c 4; f5 = c 5
      f6 = c 6
      j2 = (f0 * f6 - f1 * f5 + f2 * f4 - f3 * f3) `mod` p
      j4 = (f0 * f4 * f6 + f1 * f3 * f5 - f0 * f5 * f5 - f1 * f1 * f6 - f2 * f3 * f4 + f2 * f2 * f6) `mod` p
      j6 = (f0 * f0 * f6 * f6 - f0 * f1 * f5 * f6 + f0 * f2 * f4 * f6 - f0 * f3 * f3 * f6 + f1 * f1 * f4 * f6 - f1 * f2 * f3 * f6) `mod` p
      j10 = hyperDiscriminant (HyperCurve coeffs 2 p)
  in IgusaInvariants ((j2 + p) `mod` p) ((j4 + p) `mod` p) ((j6 + p) `mod` p) ((j10 + p) `mod` p)

igusaClebsch :: HyperCurve -> (Integer, Integer, Integer)
igusaClebsch hc =
  let p = hcPrime hc
      IgusaInvariants j2 j4 j6 j10 = igusaInvariants hc
      i1 = if j4 /= 0 then (j2 * j2 `mod` p * modInv j4 p) `mod` p else 0
      i2 = if j6 /= 0 then (modPow j2 3 p * modInv j6 p) `mod` p else 0
      i3 = if j10 /= 0 then (modPow j2 5 p * modInv j10 p) `mod` p else 0
  in (i1, i2, i3)

-- ============================================================
-- Coefficient Routing via Jacobian
-- ============================================================

coeffsToJacobian :: Integer -> [Integer] -> [MumfordDiv]
coeffsToJacobian p coeffs =
  let pairs = pairUp coeffs
  in map (\(a, b) -> MumfordDiv (iToV [b `mod` p, a `mod` p, 1]) (V.singleton 0) p) pairs
  where
    pairUp [] = []
    pairUp [x] = [(x, 0)]
    pairUp (a:b:rest) = (a, b) : pairUp rest

jacobianToCoeffs :: [MumfordDiv] -> [Integer]
jacobianToCoeffs = concatMap (\(MumfordDiv u _ _) ->
  let l = vToI u
  in case l of
    [b, a, _] -> [a, b]
    [b, _]    -> [0, b]
    [b]       -> [0, b]
    _         -> [0, 0]
  )

mixViaJacobian :: HyperCurve -> [Integer] -> [Integer]
mixViaJacobian hc coeffs =
  let p = hcPrime hc
      divs = coeffsToJacobian p coeffs
      accum = foldl' (jacobianAdd hc) (jacobianIdentity p) divs
      MumfordDiv u v _ = accum
      mixU = polyEval p u 1
      mixV = polyEval p v 1
  in zipWith (\i c -> (c + mixU * fromIntegral i + mixV) `mod` p) [0 :: Int ..] coeffs
