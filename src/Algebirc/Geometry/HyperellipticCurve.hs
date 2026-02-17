-- |
-- Module      : Algebirc.Geometry.HyperellipticCurve
-- Description : Genus-2 hyperelliptic curves, Jacobian arithmetic, Igusa invariants
-- License     : MIT
--
-- = Mathematical Foundation
--
-- A genus-2 hyperelliptic curve: C: y² = f(x) where deg(f) ∈ {5, 6} over GF(p).
--
-- The Jacobian Jac(C) is a 2-dimensional abelian variety.
-- Points on Jac(C) are represented via Mumford coordinates:
--   D = (u(x), v(x)) where u is monic, deg(u) ≤ 2, deg(v) < deg(u), u | v²-f.
--
-- Igusa invariants (J₂, J₄, J₆, J₁₀) classify genus-2 curves
-- up to isomorphism over algebraically closed fields.

module Algebirc.Geometry.HyperellipticCurve
  ( -- * Curve Construction
    mkHyperCurve
  , hyperDiscriminant
    -- * Polynomial Arithmetic (in GF(p))
  , polyAdd
  , polyMul
  , polyMod
  , polyDiv
  , polyEval
  , polyLeadCoeff
  , polyDeg
    -- * Jacobian Arithmetic (Cantor)
  , jacobianAdd
  , jacobianDouble
  , jacobianNegate
  , jacobianScalarMul
  , jacobianIdentity
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

-- ============================================================
-- Polynomial Arithmetic in GF(p)[x]
-- ============================================================

-- All polynomials are represented as lists of coefficients
-- in ascending degree order: [a₀, a₁, a₂, ...] = a₀ + a₁x + a₂x² + ...

-- | Normalize: remove trailing zeros, reduce mod p.
polyNorm :: Integer -> [Integer] -> [Integer]
polyNorm p = dropTrailingZeros . map (`mod` p)
  where
    dropTrailingZeros xs = 
      let reversed = dropWhile (== 0) (reverse xs)
      in if null reversed then [0] else reverse reversed

-- | Polynomial degree (-1 for zero polynomial).
polyDeg :: [Integer] -> Int
polyDeg [0] = -1
polyDeg xs  = length xs - 1

-- | Leading coefficient.
polyLeadCoeff :: [Integer] -> Integer
polyLeadCoeff [] = 0
polyLeadCoeff xs = last xs

-- | Polynomial addition in GF(p).
polyAdd :: Integer -> [Integer] -> [Integer] -> [Integer]
polyAdd p as bs =
  let n = max (length as) (length bs)
      as' = as ++ replicate (n - length as) 0
      bs' = bs ++ replicate (n - length bs) 0
  in polyNorm p $ zipWith (\a b -> (a + b) `mod` p) as' bs'

-- | Polynomial subtraction in GF(p).
polySub :: Integer -> [Integer] -> [Integer] -> [Integer]
polySub p as bs =
  let n = max (length as) (length bs)
      as' = as ++ replicate (n - length as) 0
      bs' = bs ++ replicate (n - length bs) 0
  in polyNorm p $ zipWith (\a b -> ((a - b) `mod` p + p) `mod` p) as' bs'

-- | Polynomial multiplication in GF(p).
polyMul :: Integer -> [Integer] -> [Integer] -> [Integer]
polyMul p [] _ = [0]
polyMul p _ [] = [0]
polyMul p as bs =
  let n = length as + length bs - 1
      result = [ sum [ (as !! i) * (bs !! (k - i))
                     | i <- [max 0 (k - length bs + 1) .. min k (length as - 1)]
                     ] `mod` p
               | k <- [0..n-1]
               ]
  in polyNorm p result

-- | Polynomial division with remainder in GF(p).
-- Returns (quotient, remainder).
polyDiv :: Integer -> [Integer] -> [Integer] -> ([Integer], [Integer])
polyDiv p num den
  | polyDeg den < 0 = error "Division by zero polynomial"
  | polyDeg num < polyDeg den = ([0], num)
  | otherwise =
      let lc = polyLeadCoeff den
          lcInv = modInv lc p
          degDiff = polyDeg num - polyDeg den
      in go (polyNorm p num) den lcInv p degDiff (replicate (degDiff + 1) 0)

go :: [Integer] -> [Integer] -> Integer -> Integer -> Int -> [Integer] -> ([Integer], [Integer])
go rem' den lcInv p degDiff quot
  | polyDeg rem' < polyDeg den = (polyNorm p quot, polyNorm p rem')
  | otherwise =
      let dd = polyDeg rem' - polyDeg den
          coeff = (polyLeadCoeff rem' * lcInv) `mod` p
          -- Construct the monomial: coeff * x^dd
          mono = replicate dd 0 ++ [coeff]
          -- Subtract mono * den from remainder
          subtracted = polyMul p mono den
          newRem = polySub p rem' subtracted
          -- Update quotient
          newQuot = polyAdd p quot mono
      in go newRem den lcInv p degDiff newQuot

-- | Polynomial modular remainder.
polyMod :: Integer -> [Integer] -> [Integer] -> [Integer]
polyMod p num den = snd (polyDiv p num den)

-- | Evaluate polynomial at a point.
polyEval :: Integer -> [Integer] -> Integer -> Integer
polyEval p coeffs x = foldl' (\acc (c, i) ->
  (acc + c * modPow x (fromIntegral i) p) `mod` p
  ) 0 (zip coeffs [0 :: Int ..])

-- | Make polynomial monic (divide by leading coefficient).
polyMakeMonic :: Integer -> [Integer] -> [Integer]
polyMakeMonic _ [0] = [0]
polyMakeMonic p xs =
  let lc = polyLeadCoeff xs
      lcInv = modInv lc p
  in polyNorm p $ map (\c -> (c * lcInv) `mod` p) xs

-- ============================================================
-- Curve Construction
-- ============================================================

-- | Smart constructor for genus-2 hyperelliptic curve.
-- Validates that deg(f) ∈ {5, 6}.
mkHyperCurve :: [Integer] -> Integer -> Either String HyperCurve
mkHyperCurve coeffs p
  | p <= 2 = Left "Prime must be > 2"
  | deg < 5 || deg > 6 = Left $ "Genus-2 requires deg(f) ∈ {5,6}, got " ++ show deg
  | otherwise = Right $ HyperCurve (map (`mod` p) coeffs) 2 p
  where
    deg = length (dropWhile (== 0) (reverse coeffs)) - 1

-- | Discriminant of genus-2 curve (simplified: product of differences of roots).
-- Non-zero discriminant ↔ non-singular curve.
hyperDiscriminant :: HyperCurve -> Integer
hyperDiscriminant (HyperCurve coeffs _ p) =
  -- Use resultant of f and f' as discriminant proxy
  let f = coeffs
      f' = polyDeriv p f
      -- Compute resultant via polynomial GCD
      g = polyGCD p f f'
  in if polyDeg g > 0 then 0 else polyLeadCoeff f `mod` p

-- | Polynomial derivative in GF(p).
polyDeriv :: Integer -> [Integer] -> [Integer]
polyDeriv p coeffs = polyNorm p
  [ (fromIntegral i * c) `mod` p | (c, i) <- zip (tail coeffs) [1 :: Int ..] ]

-- | Polynomial GCD in GF(p) via Euclidean algorithm.
polyGCD :: Integer -> [Integer] -> [Integer] -> [Integer]
polyGCD p a b
  | polyDeg b < 0 = polyMakeMonic p a
  | otherwise = polyGCD p b (polyMod p a b)

-- ============================================================
-- Jacobian Arithmetic (Cantor's Algorithm)
-- ============================================================

-- | The identity element on Jac(C): D₀ = (1, 0) ↔ divisor class [0].
jacobianIdentity :: Integer -> MumfordDiv
jacobianIdentity p = MumfordDiv [1] [0] p

-- | Negate a divisor: -(u, v) = (u, -v mod f mod u).
jacobianNegate :: HyperCurve -> MumfordDiv -> MumfordDiv
jacobianNegate (HyperCurve _ _ p) (MumfordDiv u v _) =
  let negV = polyNorm p $ map (\c -> (p - c) `mod` p) v
  in MumfordDiv u negV p

-- | Cantor's algorithm: add two divisors on Jac(C).
-- Input: D₁ = (u₁, v₁), D₂ = (u₂, v₂) on C: y² = f(x).
-- Output: D₃ = (u₃, v₃) = D₁ + D₂ (reduced).
jacobianAdd :: HyperCurve -> MumfordDiv -> MumfordDiv -> MumfordDiv
jacobianAdd hc d1 d2
  | isIdentity d1 = d2
  | isIdentity d2 = d1
  | otherwise = cantorCompose hc d1 d2

-- | Check if a divisor is the identity.
isIdentity :: MumfordDiv -> Bool
isIdentity (MumfordDiv [1] _ _) = True
isIdentity (MumfordDiv u _ _)   = polyDeg u <= 0 && head u == 1

-- | Cantor composition + reduction.
cantorCompose :: HyperCurve -> MumfordDiv -> MumfordDiv -> MumfordDiv
cantorCompose (HyperCurve fCoeffs g p) (MumfordDiv u1 v1 _) (MumfordDiv u2 v2 _) =
  -- Step 1: Extended GCD of u₁, u₂
  let (d1, e1, e2) = polyExtGCD p u1 u2
      -- Step 2: Combine with (v₁ + v₂)
      vSum = polyAdd p v1 v2
      (d, c1, c2) = polyExtGCD p d1 vSum
      -- Step 3: Compose
      -- s₁ = c₁·e₁, s₂ = c₁·e₂, s₃ = c₂
      s1 = polyMul p c1 e1
      s2 = polyMul p c1 e2
      s3 = [c2 !! 0 | not (null c2)]  -- scalar
      -- Step 4: u₃ = (u₁·u₂) / d²
      u1u2 = polyMul p u1 u2
      d2 = polyMul p d d
      (uNew, _) = polyDiv p u1u2 (if polyDeg d2 > 0 then d2 else [1])
      -- Step 5: v₃ = (s₁·u₁·v₂ + s₂·u₂·v₁ + s₃·(v₁·v₂ + f)) / d
      t1 = polyMul p (polyMul p s1 u1) v2
      t2 = polyMul p (polyMul p s2 u2) v1
      v1v2 = polyMul p v1 v2
      t3inner = polyAdd p v1v2 fCoeffs
      t3 = if null s3 then [0] else polyMul p s3 t3inner
      vNewNum = polyAdd p (polyAdd p t1 t2) t3
      (vNew, _) = if polyDeg d > 0 then polyDiv p vNewNum d else (vNewNum, [0])
      -- Step 6: Reduce mod u₃
      vReduced = polyMod p vNew uNew
  in cantorReduce (HyperCurve fCoeffs g p) (MumfordDiv (polyMakeMonic p uNew) (polyNorm p vReduced) p)

-- | Cantor reduction: ensure deg(u) ≤ g.
cantorReduce :: HyperCurve -> MumfordDiv -> MumfordDiv
cantorReduce (HyperCurve fCoeffs g p) (MumfordDiv u v _)
  | polyDeg u <= g = MumfordDiv (polyMakeMonic p u) v p
  | otherwise =
      -- u' = (f - v²) / u
      let v2 = polyMul p v v
          fMinusV2 = polySub p fCoeffs v2
          (uNew, _) = polyDiv p fMinusV2 u
          -- v' = -v mod u'
          negV = polyNorm p $ map (\c -> (p - c) `mod` p) v
          vNew = polyMod p negV uNew
      in cantorReduce (HyperCurve fCoeffs g p) (MumfordDiv (polyMakeMonic p uNew) (polyNorm p vNew) p)

-- | Doubling on Jacobian: 2·D.
jacobianDouble :: HyperCurve -> MumfordDiv -> MumfordDiv
jacobianDouble hc d = jacobianAdd hc d d

-- | Scalar multiplication on Jacobian: n·D via double-and-add.
jacobianScalarMul :: HyperCurve -> Integer -> MumfordDiv -> MumfordDiv
jacobianScalarMul hc n d
  | n == 0    = jacobianIdentity (hcPrime hc)
  | n == 1    = d
  | n < 0     = jacobianScalarMul hc (-n) (jacobianNegate hc d)
  | even n    = let half = jacobianScalarMul hc (n `div` 2) d
                in jacobianDouble hc half
  | otherwise = jacobianAdd hc d (jacobianScalarMul hc (n - 1) d)

-- | Extended GCD for polynomials in GF(p).
polyExtGCD :: Integer -> [Integer] -> [Integer] -> ([Integer], [Integer], [Integer])
polyExtGCD p a b
  | polyDeg b < 0 = (polyMakeMonic p a, [1], [0])
  | otherwise =
      let (q, r) = polyDiv p a b
          (g, s, t) = polyExtGCD p b r
      in (g, t, polySub p s (polyMul p q t))

-- ============================================================
-- Igusa Invariants
-- ============================================================

-- | Compute Igusa invariants (J₂, J₄, J₆, J₁₀) from a genus-2 curve.
-- For C: y² = f₆x⁶ + f₅x⁵ + f₄x⁴ + f₃x³ + f₂x² + f₁x + f₀.
igusaInvariants :: HyperCurve -> IgusaInvariants
igusaInvariants (HyperCurve coeffs _ p) =
  let -- Pad/extract coefficients
      c i = if i < length coeffs then (coeffs !! i) `mod` p else 0
      f0 = c 0; f1 = c 1; f2 = c 2; f3 = c 3; f4 = c 4; f5 = c 5
      f6 = if length coeffs > 6 then c 6 else 0
      
      -- Simplified Igusa-Clebsch invariants (using Streng's formulas)
      -- J₂ (weight 2) — related to the sum of products of pairs of roots
      j2 = (f0 * f6 - f1 * f5 + f2 * f4 - f3 * f3) `mod` p
      
      -- J₄ (weight 4) — quartic invariant
      j4 = (f0 * f4 * f6 + f1 * f3 * f5 
           - f0 * f5 * f5 - f1 * f1 * f6
           - f2 * f3 * f4 + f2 * f2 * f6) `mod` p
      
      -- J₆ (weight 6) — sextic invariant
      j6 = (f0 * f0 * f6 * f6 - f0 * f1 * f5 * f6
           + f0 * f2 * f4 * f6 - f0 * f3 * f3 * f6
           + f1 * f1 * f4 * f6 - f1 * f2 * f3 * f6) `mod` p
      
      -- J₁₀ (weight 10) — discriminant-like
      j10 = hyperDiscriminant (HyperCurve coeffs 2 p)
      
  in IgusaInvariants ((j2 + p) `mod` p) ((j4 + p) `mod` p)
                      ((j6 + p) `mod` p) ((j10 + p) `mod` p)

-- | Igusa-Clebsch invariants (alternative formulation).
-- Returns absolute invariants: (i₁, i₂, i₃) = (J₂²/J₄, J₂³/J₆, J₂⁵/J₁₀).
igusaClebsch :: HyperCurve -> (Integer, Integer, Integer)
igusaClebsch hc =
  let p = hcPrime hc
      IgusaInvariants j2 j4 j6 j10 = igusaInvariants hc
      -- Absolute invariants (division in GF(p))
      i1 = if j4 /= 0 then (j2 * j2 `mod` p * modInv j4 p) `mod` p else 0
      i2 = if j6 /= 0 then (modPow j2 3 p * modInv j6 p) `mod` p else 0
      i3 = if j10 /= 0 then (modPow j2 5 p * modInv j10 p) `mod` p else 0
  in (i1, i2, i3)

-- ============================================================
-- Coefficient Routing via Jacobian
-- ============================================================

-- | Convert pairs of coefficients to Jacobian divisors.
-- Each pair (a, b) becomes a Mumford divisor (x² + ax + b, 0).
coeffsToJacobian :: Integer -> [Integer] -> [MumfordDiv]
coeffsToJacobian p coeffs =
  let pairs = pairUp coeffs
  in map (\(a, b) -> MumfordDiv [b `mod` p, a `mod` p, 1] [0] p) pairs
  where
    pairUp [] = []
    pairUp [x] = [(x, 0)]
    pairUp (a:b:rest) = (a, b) : pairUp rest

-- | Convert Jacobian divisors back to coefficient pairs.
jacobianToCoeffs :: [MumfordDiv] -> [Integer]
jacobianToCoeffs = concatMap (\(MumfordDiv u _ _) ->
  case u of
    [b, a, _] -> [a, b]
    [b, _]    -> [0, b]
    [b]       -> [0, b]
    _         -> [0, 0]
  )

-- | Mix coefficients via Jacobian arithmetic:
-- 1. Convert pairs to divisors
-- 2. Add all divisors together on Jac(C)
-- 3. Use the accumulated divisor to derive mixing constants
-- 4. XOR-fold back into the coefficient stream
mixViaJacobian :: HyperCurve -> [Integer] -> [Integer]
mixViaJacobian hc coeffs =
  let p = hcPrime hc
      divs = coeffsToJacobian p coeffs
      -- Accumulate divisors
      accum = foldl' (jacobianAdd hc) (jacobianIdentity p) divs
      -- Extract mixing constants from the accumulated divisor
      MumfordDiv u v _ = accum
      mixU = polyEval p u 1  -- evaluate u at special points
      mixV = polyEval p v 1
      -- Apply mixing: c'_i = (c_i + mixU * i + mixV) mod p
  in zipWith (\i c ->
    (c + mixU * fromIntegral i + mixV) `mod` p
    ) [0 :: Int ..] coeffs
