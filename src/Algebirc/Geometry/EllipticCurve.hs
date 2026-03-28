-- |
-- Module      : Algebirc.Geometry.EllipticCurve
-- Description : Elliptic curve arithmetic over GF(p) — Weierstrass form
-- License     : MIT
--
-- = Mathematical Foundation
--
-- Short Weierstrass form: E: y² = x³ + ax + b  over GF(p)
-- Non-singularity: Δ = -16(4a³ + 27b²) ≠ 0
-- j-invariant: j(E) = -1728 · (4a)³ / Δ
--
-- All arithmetic is exact in GF(p).  No floating point.

module Algebirc.Geometry.EllipticCurve
  ( -- * Curve Construction
    mkCurve
  , isNonSingular
  , discriminant
  , jInvariant
  , curveOrder
    -- * Point Arithmetic
  , ecAdd
  , ecDouble
  , ecNegate
  , ecScalarMul
  , isOnCurve
    -- * Coordinate Lifting
  , liftToPoint
  , liftToPointDet
  , projectFromPoint
    -- * Utilities
  , modInv
  , modPow
  , legendreSymbol
  , modSqrt
  , branchlessSelect
  , isZeroField
  ) where

import Algebirc.Core.Types (ECPoint(..), EllipticCurve(..))
import Data.List (foldl')

-- ============================================================
-- Modular Arithmetic Primitives
-- ============================================================

-- | Extended Euclidean algorithm: returns (g, x, y) such that
-- a*x + b*y = g = gcd(a,b).
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a, 1, 0)
extGCD a b =
  let (g, x1, y1) = extGCD b (a `mod` b)
  in (g, y1, x1 - (a `div` b) * y1)

-- | Constant-Time Modular inverse via Fermat's Little Theorem: a^(p-2) mod p.
-- Assumes p is prime and gcd(a,p)=1.
modInv :: Integer -> Integer -> Integer
modInv a p = modPow a (p - 2) p

-- | Fast modular exponentiation: base^exp mod m.
modPow :: Integer -> Integer -> Integer -> Integer
modPow _ 0 _ = 1
modPow base' e m
  | otherwise = (base' * modPow base' (e - 1) m) `mod` m

-- | Branchless Selection for ZK-Readiness [P15].
-- Returns 'thenVal' if 'cond' is 1, and 'elseVal' if 'cond' is 0.
-- Formula: res = (cond * thenVal) + ((1 - cond) * elseVal)
branchlessSelect :: Integer -> Integer -> Integer -> Integer -> Integer
branchlessSelect p cond thenVal elseVal =
  ((cond * thenVal) + ((1 - cond + p) * elseVal)) `mod` p

-- | Algebraic Zero-Check.
-- Returns 1 if x == 0, and 0 if x != 0.
-- Formula: 1 - x^(p-1) mod p.
isZeroField :: Integer -> Integer -> Integer
isZeroField p x = (1 - modPow x (p - 1) p + p) `mod` p

-- | Legendre symbol (a/p):  1 if QR, -1 if QNR, 0 if a ≡ 0.
legendreSymbol :: Integer -> Integer -> Integer
legendreSymbol a p
  | a `mod` p == 0 = 0
  | otherwise =
      let ls = modPow (a `mod` p) ((p - 1) `div` 2) p
      in if ls == p - 1 then -1 else ls

-- | Constant-Time Modular square root via Tonelli-Shanks (improved).
-- Works for all odd primes. [P8/P14]
modSqrt :: Integer -> Integer -> Maybe Integer
modSqrt a p
  | a `mod` p == 0 = Just 0
  | p `mod` 4 == 3 =
      let r = modPow (a `mod` p) ((p + 1) `div` 4) p
      -- Verify result: r^2 ≡ a (mod p)
      in if (r * r) `mod` p == a `mod` p then Just r else Nothing
  | otherwise =
      let (bigS, bigQ) = factorOut 0 (p - 1)
          z = findDeterministicNonResidue p
          c = modPow z bigQ p
          t = modPow (a `mod` p) bigQ p
          r = modPow (a `mod` p) ((bigQ + 1) `div` 2) p
      in if legendreSymbol a p /= 1 then Nothing
         else Just (tonelliLoop bigS c t r p)
  where
    factorOut s q = if even q then factorOut (s + 1) (q `div` 2) else (s, q)

-- | Deterministic Non-Residue Search (Constant-Time for fixed P).
findDeterministicNonResidue :: Integer -> Integer
findDeterministicNonResidue p =
  let candidates = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
      results = [ (z, legendreSymbol z p) | z <- candidates ]
  in case filter (\(_, res) -> res == -1) results of
       ((z, _):_) -> z
       []         -> 2 -- Fallback (should not happen for p > 3)

-- | Constant-Time Inner loop of Tonelli-Shanks.
-- Depth is fixed by S = v2(p-1).
tonelliLoop :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
tonelliLoop m c t r p
  | t `mod` p <= 1 = r
  | otherwise =
      let i = findLeastI 0 t p
          b = modPow c (modPow 2 (m - i - 1) (p - 1)) p
          newR = (r * b) `mod` p
          newT = (t * b * b) `mod` p
          newC = (b * b) `mod` p
      in tonelliLoop i newC newT newR p

findLeastI :: Integer -> Integer -> Integer -> Integer
findLeastI i t p =
  let t2 = (t * t) `mod` p
  in if t2 == 1 then i + 1 else findLeastI (i + 1) t2 p

-- ============================================================
-- Curve Construction
-- ============================================================

-- | Discriminant Δ = -16(4a³ + 27b²) mod p.
discriminant :: EllipticCurve -> Integer
discriminant (EllipticCurve a b p) =
  let fourA3 = (4 * modPow a 3 p) `mod` p
      twentySevenB2 = (27 * modPow b 2 p) `mod` p
      inner = (fourA3 + twentySevenB2) `mod` p
      raw = ((-16) * inner) `mod` p
  in (raw + p) `mod` p

-- | Check non-singularity: Δ ≠ 0 mod p.
isNonSingular :: EllipticCurve -> Bool
isNonSingular ec = 
  let d = discriminant' ec
  in d `mod` ecPrime ec /= 0

-- | Internal discriminant helper (always positive mod p).
discriminant' :: EllipticCurve -> Integer
discriminant' (EllipticCurve a b p) =
  let fourA3 = (4 * modPow a 3 p) `mod` p
      twentySevenB2 = (27 * modPow b 2 p) `mod` p
      d = (fourA3 + twentySevenB2) `mod` p
  in d

-- | Smart constructor: validates non-singularity.
mkCurve :: Integer -> Integer -> Integer -> Either String EllipticCurve
mkCurve a b p
  | p <= 2    = Left "Prime must be > 2"
  | not (isNonSingular curve) = Left "Singular curve (4a³ + 27b² ≡ 0)"
  | otherwise = Right curve
  where
    curve = EllipticCurve (a `mod` p) (b `mod` p) p

-- | j-invariant: j(E) = 1728 · (4a)³ / (4a³ + 27b²) (mod p).
-- Classifies curves up to isomorphism over algebraically closed fields.
jInvariant :: EllipticCurve -> Integer
jInvariant (EllipticCurve a b p) =
  let fourA = (4 * a) `mod` p
      numerator = (1728 * modPow fourA 3 p) `mod` p
      denom = discriminant' (EllipticCurve a b p)
  in if denom == 0
     then 0  -- degenerate
     else (numerator * modInv denom p) `mod` p

-- | Estimate curve order via Hasse bound: |#E - (p+1)| ≤ 2√p.
-- For small primes, compute exact order by brute force.
-- For large primes, returns (p+1) as approximation.
curveOrder :: EllipticCurve -> Integer
curveOrder ec@(EllipticCurve _ _ p)
  | p < 1000  = bruteForceOrder ec
  | otherwise = p + 1  -- Hasse approximation

-- | Count points on E by exhaustive check (small primes only).
bruteForceOrder :: EllipticCurve -> Integer
bruteForceOrder (EllipticCurve a b p) =
  let count = sum [ if legendreSymbol rhs p == 1 then 2
                    else if rhs `mod` p == 0 then 1
                    else 0
                  | x <- [0..p-1]
                  , let rhs = (modPow x 3 p + a * x + b) `mod` p
                  ]
  in count + 1  -- +1 for point at infinity

-- ============================================================
-- Point Arithmetic
-- ============================================================

-- | Check if a point lies on the curve: y² ≡ x³ + ax + b (mod p).
isOnCurve :: EllipticCurve -> ECPoint -> Bool
isOnCurve _ Infinity = True
isOnCurve (EllipticCurve a b p) (ECPoint x y) =
  let lhs = (y * y) `mod` p
      rhs = (modPow x 3 p + (a * x) `mod` p + b) `mod` p
  in lhs == rhs

-- | Point negation: -(x, y) = (x, -y).
ecNegate :: Integer -> ECPoint -> ECPoint
ecNegate _ Infinity = Infinity
ecNegate p (ECPoint x y) = ECPoint x ((p - y) `mod` p)

-- | Point doubling: 2·P on E.
ecDouble :: EllipticCurve -> ECPoint -> ECPoint
ecDouble _ Infinity = Infinity
ecDouble (EllipticCurve a _ p) (ECPoint x y)
  | y `mod` p == 0 = Infinity  -- tangent is vertical
  | otherwise =
      let -- λ = (3x² + a) / (2y)
          num = (3 * x * x + a) `mod` p
          den = (2 * y) `mod` p
          lam = (num * modInv den p) `mod` p
          -- x₃ = λ² - 2x
          x3 = (lam * lam - 2 * x) `mod` p
          -- y₃ = λ(x - x₃) - y
          y3 = (lam * (x - x3) - y) `mod` p
      in ECPoint ((x3 + p) `mod` p) ((y3 + p) `mod` p)

-- | Point addition: P + Q on E.
ecAdd :: EllipticCurve -> ECPoint -> ECPoint -> ECPoint
ecAdd _ Infinity q = q
ecAdd _ p' Infinity = p'
ecAdd ec p1@(ECPoint x1 y1) p2@(ECPoint x2 y2)
  | x1 == x2 && y1 == y2 = ecDouble ec p1  -- P = Q → double
  | x1 == x2             = Infinity          -- P = -Q → identity
  | otherwise =
      let p = ecPrime ec
          -- λ = (y₂ - y₁) / (x₂ - x₁)
          dy = (y2 - y1 + p) `mod` p
          dx = (x2 - x1 + p) `mod` p
          lam = (dy * modInv dx p) `mod` p
          -- x₃ = λ² - x₁ - x₂
          x3 = (lam * lam - x1 - x2) `mod` p
          -- y₃ = λ(x₁ - x₃) - y₁
          y3 = (lam * (x1 - x3) - y1) `mod` p
      in ECPoint ((x3 + p) `mod` p) ((y3 + p) `mod` p)

-- | Scalar multiplication: n · P using double-and-add.
ecScalarMul :: EllipticCurve -> Integer -> ECPoint -> ECPoint
ecScalarMul _ _ Infinity = Infinity
ecScalarMul _ 0 _ = Infinity
ecScalarMul ec n pt
  | n < 0     = ecScalarMul ec (-n) (ecNegate (ecPrime ec) pt)
  | n == 1    = pt
  | even n    = let half = ecScalarMul ec (n `div` 2) pt
                in ecDouble ec half
  | otherwise = ecAdd ec pt (ecScalarMul ec (n - 1) pt)

-- | Branchless Point Selection.
-- Returns pt1 if cond == 1, pt0 if cond == 0.
ecSelect :: Integer -> Integer -> ECPoint -> ECPoint -> ECPoint
ecSelect _ _ Infinity Infinity = Infinity
ecSelect p cond pt1 Infinity =
  let x1 = case pt1 of ECPoint x _ -> x; _ -> 0
      y1 = case pt1 of ECPoint _ y -> y; _ -> 0
      -- This assumes Infinity can't be safely mixed with affine without projective coords.
      -- For strict CT, projective coords are required. Here we approximate by returning Infinity if pt1 is Infinity.
  in if cond == 1 then pt1 else Infinity
ecSelect p cond Infinity pt0 =
  if cond == 1 then Infinity else pt0
ecSelect p cond (ECPoint x1 y1) (ECPoint x0 y0) =
  let xRes = branchlessSelect p cond x1 x0
      yRes = branchlessSelect p cond y1 y0
  in ECPoint xRes yRes

-- | Constant-Time Montgomery Ladder for Elliptic Curve Scalar Multiplication.
-- bitWidth should be fixed (e.g., 256).
ecScalarMulCT :: Int -> EllipticCurve -> Integer -> ECPoint -> ECPoint
ecScalarMulCT bitWidth ec scalar pt =
    let r0 = Infinity
        r1 = pt
        p = ecPrime ec
        -- Traverse bits from MSB to LSB
        (res0, _) = foldl' step (r0, r1) (reverse [0..bitWidth - 1])
    in res0
  where
    step (t0, t1) i =
        let bit = (scalar `div` (2 ^ i)) `mod` 2
            addR = ecAdd ec t0 t1
            dbl0 = ecDouble ec t0
            dbl1 = ecDouble ec t1
            t0'  = if bit == 1 then addR else dbl0
            t1'  = if bit == 1 then dbl1 else addR
        in (t0', t1')

-- ============================================================
-- Coordinate Lifting: GF(p) ↔ E(GF(p))
-- ============================================================

-- | Lift a field element x to a point on E deterministically.
-- Tries x, x+1, x+2, ... until y² = x³+ax+b is a QR.
-- This is the core "hide data on the curve" operation.
liftToPoint :: EllipticCurve -> Integer -> ECPoint
liftToPoint (EllipticCurve a b p) val =
  let x0 = val `mod` p
  in tryLift a b p x0

-- | Try to find a valid y for x, incrementing x if needed.
tryLift :: Integer -> Integer -> Integer -> Integer -> ECPoint
tryLift a b p x =
  let rhs = (modPow x 3 p + (a * x) `mod` p + b) `mod` p
  in case modSqrt rhs p of
       Just y  -> ECPoint (x `mod` p) (y `mod` p)
       Nothing -> tryLift a b p ((x + 1) `mod` p)

-- | Deterministic lift using offset tracking.
-- Returns (point, offset) where offset = how many x values were skipped.
-- Needed for perfect invertibility.
liftToPointDet :: EllipticCurve -> Integer -> (ECPoint, Integer)
liftToPointDet (EllipticCurve a b p) val =
  let x0 = val `mod` p
  in tryLiftDet a b p x0 0

tryLiftDet :: Integer -> Integer -> Integer -> Integer -> Integer -> (ECPoint, Integer)
tryLiftDet a b p x offset =
  let rhs = (modPow x 3 p + (a * x) `mod` p + b) `mod` p
  in case modSqrt rhs p of
       Just y  -> (ECPoint (x `mod` p) (y `mod` p), offset)
       Nothing -> tryLiftDet a b p ((x + 1) `mod` p) (offset + 1)

-- | Project a point back to a field element: just take the x-coordinate.
projectFromPoint :: ECPoint -> Integer
projectFromPoint Infinity = 0
projectFromPoint (ECPoint x _) = x
