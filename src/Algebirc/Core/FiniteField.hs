-- |
-- Module      : Algebirc.Core.FiniteField
-- Description : Finite field GF(p) arithmetic
-- License     : MIT
--
-- = Formal Properties
--
-- For prime @p@, GF(p) satisfies:
--
-- 1. __Closure__: ∀a,b ∈ GF(p). a ⊕ b ∈ GF(p), a ⊗ b ∈ GF(p)
-- 2. __Associativity__: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
-- 3. __Identity__: a ⊕ 0 = a, a ⊗ 1 = a
-- 4. __Inverse__: ∀a. ∃(-a). a ⊕ (-a) = 0; ∀a≠0. ∃a⁻¹. a ⊗ a⁻¹ = 1
-- 5. __Distributivity__: a ⊗ (b ⊕ c) = (a ⊗ b) ⊕ (a ⊗ c)

module Algebirc.Core.FiniteField
  ( -- * Arithmetic
    ffAdd
  , ffSub
  , ffMul
  , ffDiv
  , ffNeg
  , ffInv
  , ffPow
    -- * Construction
  , ffFromInteger
  , ffFromBytes
  , ffZero
  , ffOne
    -- * Properties
  , ffIsZero
  , ffOrder
    -- * Extended Euclidean
  , extGcd
  ) where

import Algebirc.Core.Types
import qualified Data.ByteString as BS

-- ============================================================
-- Construction
-- ============================================================

-- | Create field element, reducing mod p.
ffFromInteger :: Integer -> Integer -> FieldElement
ffFromInteger = mkFieldElement

-- | Zero element of GF(p).
ffZero :: Integer -> FieldElement
ffZero p = mkFieldElement 0 p

-- | Multiplicative identity of GF(p).
ffOne :: Integer -> FieldElement
ffOne p = mkFieldElement 1 p

-- | Convert a bytestring to a field element.
-- Interprets bytes as big-endian integer, then reduces mod p.
ffFromBytes :: BS.ByteString -> Integer -> FieldElement
ffFromBytes bs p =
  let val = BS.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 bs
  in mkFieldElement val p

-- ============================================================
-- Predicates
-- ============================================================

ffIsZero :: FieldElement -> Bool
ffIsZero (FieldElement v _) = v == 0

-- | Multiplicative order of an element (smallest k > 0 s.t. a^k = 1).
-- Returns Nothing for zero element.
ffOrder :: FieldElement -> Maybe Integer
ffOrder fe
  | ffIsZero fe = Nothing
  | otherwise   = Just $ go 1 fe
  where
    one = ffOne (feModulus fe)
    go k acc
      | acc == one = k
      | k > feModulus fe = k  -- safety: shouldn't happen for prime field
      | otherwise = go (k + 1) (ffMul acc fe)

-- ============================================================
-- Arithmetic
-- ============================================================

-- | Ensure two elements are in the same field.
checkField :: FieldElement -> FieldElement -> Integer
checkField a b
  | feModulus a /= feModulus b = error $
      "FieldMismatch: " ++ show (feModulus a) ++ " vs " ++ show (feModulus b)
  | otherwise = feModulus a

-- | Addition in GF(p).
ffAdd :: FieldElement -> FieldElement -> FieldElement
ffAdd a b =
  let p = checkField a b
  in mkFieldElement (feValue a + feValue b) p

-- | Subtraction in GF(p).
ffSub :: FieldElement -> FieldElement -> FieldElement
ffSub a b =
  let p = checkField a b
  in mkFieldElement (feValue a - feValue b + p) p

-- | Multiplication in GF(p).
ffMul :: FieldElement -> FieldElement -> FieldElement
ffMul a b =
  let p = checkField a b
  in mkFieldElement (feValue a * feValue b) p

-- | Additive inverse (negation).
ffNeg :: FieldElement -> FieldElement
ffNeg (FieldElement v p) = mkFieldElement (p - v) p

-- | Multiplicative inverse via extended Euclidean algorithm.
--
-- For prime p and a /= 0: a * a^(-1) ≡ 1 (mod p)
-- Uses: a^(-1) = a^(p-2) (mod p) by Fermat's little theorem,
-- but we use extGcd for efficiency.
ffInv :: FieldElement -> Either AlgebircError FieldElement
ffInv (FieldElement 0 _) = Left DivisionByZero
ffInv (FieldElement v p) =
  let (g, x, _) = extGcd v p
  in if g /= 1
     then Left (InverseNotFound $ "gcd(" ++ show v ++ "," ++ show p ++ ") = " ++ show g)
     else Right (mkFieldElement x p)

-- | Division: a / b = a * b^(-1).
ffDiv :: FieldElement -> FieldElement -> Either AlgebircError FieldElement
ffDiv a b = do
  let _ = checkField a b
  bInv <- ffInv b
  return (ffMul a bInv)

-- | Modular exponentiation via square-and-multiply.
--
-- Complexity: O(log n) multiplications.
ffPow :: FieldElement -> Integer -> FieldElement
ffPow _ 0 = ffOne (feModulus (FieldElement 0 2))  -- fallback, but shouldn't be called this way
ffPow base n
  | n < 0     = case ffInv base of
                  Left _     -> error "ffPow: cannot invert zero"
                  Right bInv -> ffPow bInv (negate n)
  | n == 0    = ffOne (feModulus base)
  | otherwise = squareAndMultiply base n
  where
    squareAndMultiply :: FieldElement -> Integer -> FieldElement
    squareAndMultiply b e
      | e == 1    = b
      | even e    = let half = squareAndMultiply b (e `div` 2)
                    in ffMul half half
      | otherwise = let half = squareAndMultiply b ((e - 1) `div` 2)
                    in ffMul b (ffMul half half)

-- ============================================================
-- Extended Euclidean Algorithm
-- ============================================================

-- | Extended GCD: returns (gcd, x, y) such that a*x + b*y = gcd(a,b).
extGcd :: Integer -> Integer -> (Integer, Integer, Integer)
extGcd 0 b = (b, 0, 1)
extGcd a b =
  let (g, x, y) = extGcd (b `mod` a) a
  in (g, y - (b `div` a) * x, x)
