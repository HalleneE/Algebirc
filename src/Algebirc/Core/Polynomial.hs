-- |
-- Module      : Algebirc.Core.Polynomial
-- Description : Degree-bounded polynomial operations with canonical normalization
-- License     : MIT
--
-- = Formal Guarantees
--
-- * __Degree bound__: @deg(f ∘ g) = deg(f) * deg(g)@.
--   If this exceeds 'polyMaxDeg', composition returns 'DegreeOverflow'.
-- * __Canonical form__: after every operation, terms are normalized:
--   sorted descending by exponent, merged, zero-stripped.
-- * __Idempotent normalization__: @normalize . normalize = normalize@.

module Algebirc.Core.Polynomial
  ( -- * Construction
    zeroPoly
  , constPoly
  , monoTerm
  , linearPoly
    -- * Operations
  , polyAdd
  , polySub
  , polyMul
  , polyScale
  , polyCompose
    -- * Evaluation
  , polyEval
  , polyEvalField
    -- * Interpolation
  , lagrangeInterpolate
    -- * Queries
  , polyIsZero
  , polyNormalize
  ) where

import Algebirc.Core.Types
import Algebirc.Core.FiniteField

-- ============================================================
-- Construction
-- ============================================================

-- | The zero polynomial.
zeroPoly :: Integer -> Int -> BoundedPoly
zeroPoly fieldMod maxDeg = mkBoundedPoly fieldMod maxDeg []

-- | Constant polynomial: c.
constPoly :: Integer -> Int -> Integer -> BoundedPoly
constPoly fieldMod maxDeg c = mkBoundedPoly fieldMod maxDeg [Term c 0]

-- | Monomial: c * x^e.
monoTerm :: Integer -> Int -> Integer -> Int -> BoundedPoly
monoTerm fieldMod maxDeg c e
  | e > maxDeg = mkBoundedPoly fieldMod maxDeg []  -- degree cap
  | otherwise  = mkBoundedPoly fieldMod maxDeg [Term c e]

-- | Linear polynomial: ax + b.
linearPoly :: Integer -> Int -> Integer -> Integer -> BoundedPoly
linearPoly fieldMod maxDeg a b =
  mkBoundedPoly fieldMod maxDeg [Term a 1, Term b 0]

-- ============================================================
-- Predicates
-- ============================================================

polyIsZero :: BoundedPoly -> Bool
polyIsZero (BoundedPoly [] _ _) = True
polyIsZero _                     = False

-- | Re-normalize (idempotent).
polyNormalize :: BoundedPoly -> BoundedPoly
polyNormalize (BoundedPoly terms maxDeg fieldMod) =
  mkBoundedPoly fieldMod maxDeg terms

-- ============================================================
-- Arithmetic
-- ============================================================

-- | Check field/degree compatibility.
checkCompat :: BoundedPoly -> BoundedPoly -> (Integer, Int)
checkCompat a b
  | polyField a /= polyField b = error $
      "Polynomial field mismatch: " ++ show (polyField a) ++ " vs " ++ show (polyField b)
  | otherwise = (polyField a, min (polyMaxDeg a) (polyMaxDeg b))

-- | Polynomial addition.
-- Degree: deg(f + g) <= max(deg f, deg g). Always safe.
polyAdd :: BoundedPoly -> BoundedPoly -> BoundedPoly
polyAdd a b =
  let (fm, md) = checkCompat a b
  in mkBoundedPoly fm md (polyTerms a ++ polyTerms b)

-- | Polynomial subtraction.
polySub :: BoundedPoly -> BoundedPoly -> BoundedPoly
polySub a b =
  let (fm, md) = checkCompat a b
      negTerms = map (\(Term c e) -> Term (negate c) e) (polyTerms b)
  in mkBoundedPoly fm md (polyTerms a ++ negTerms)

-- | Scalar multiplication.
polyScale :: Integer -> BoundedPoly -> BoundedPoly
polyScale s (BoundedPoly terms maxDeg fm) =
  mkBoundedPoly fm maxDeg (map (\(Term c e) -> Term (s * c) e) terms)

-- | Polynomial multiplication.
-- Degree: deg(f * g) = deg(f) + deg(g).
-- Returns DegreeOverflow if result exceeds cap.
polyMul :: BoundedPoly -> BoundedPoly -> Either AlgebircError BoundedPoly
polyMul a b =
  let (fm, md) = checkCompat a b
      resultDeg = polyDegree a + polyDegree b
  in if resultDeg > md
     then Left (DegreeOverflow resultDeg md)
     else
       let terms = [ Term (ca * cb) (ea + eb)
                   | Term ca ea <- polyTerms a
                   , Term cb eb <- polyTerms b
                   ]
       in Right (mkBoundedPoly fm md terms)

-- | Polynomial composition: f(g(x)).
-- Degree: deg(f ∘ g) = deg(f) * deg(g).
--
-- __This is where degree explosion happens.__
-- We check BEFORE computing and reject if it would overflow.
polyCompose :: BoundedPoly -> BoundedPoly -> Either AlgebircError BoundedPoly
polyCompose f g =
  let (fm, md) = checkCompat f g
      resultDeg = polyDegree f * max 1 (polyDegree g)
  in if resultDeg > md
     then Left (DegreeOverflow resultDeg md)
     else
       -- Horner-like composition: f(g(x))
       -- f = a_n x^n + ... + a_0
       -- f(g) = a_n * g^n + ... + a_0
       let terms = polyTerms f
           evalTerm (Term c e) =
             let gPow = polyPower g e fm md
             in case gPow of
                  Left err -> Left err
                  Right gp -> Right (polyScale c gp)
       in case mapM evalTerm terms of
            Left err -> Left err
            Right polys -> Right $ foldl polyAdd (zeroPoly fm md) polys

-- | Internal: compute g^e with degree checking.
polyPower :: BoundedPoly -> Int -> Integer -> Int -> Either AlgebircError BoundedPoly
polyPower _ 0 fm md = Right (constPoly fm md 1)
polyPower g 1 _  _  = Right g
polyPower g e fm md
  | even e    = do
      half <- polyPower g (e `div` 2) fm md
      polyMul half half
  | otherwise = do
      half <- polyPower g ((e - 1) `div` 2) fm md
      sq   <- polyMul half half
      polyMul sq g

-- ============================================================
-- Evaluation
-- ============================================================

-- | Evaluate polynomial at a point.
-- f(x) = ∑ c_i * x^e_i
-- Direct evaluation: compute each term and sum.
polyEval :: BoundedPoly -> Integer -> Integer
polyEval (BoundedPoly [] _ _) _ = 0
polyEval poly x =
  let fm = polyField poly
      evalTerm (Term c e) =
        let xPow = if fm > 0 then powModInt x e fm else x ^ e
            val  = c * xPow
        in if fm > 0 then val `mod` fm else val
      total = sum (map evalTerm (polyTerms poly))
  in if fm > 0 then ((total `mod` fm) + fm) `mod` fm else total

-- | Modular exponentiation for polynomial evaluation.
powModInt :: Integer -> Int -> Integer -> Integer
powModInt _ 0 _ = 1
powModInt base e m
  | e < 0     = error "powModInt: negative exponent"
  | even e    = let half = powModInt base (e `div` 2) m
                in (half * half) `mod` m
  | otherwise = (base * powModInt base (e - 1) m) `mod` m

-- | Evaluate polynomial at a FieldElement.
polyEvalField :: BoundedPoly -> FieldElement -> FieldElement
polyEvalField poly fe =
  let result = polyEval poly (feValue fe)
      p = feModulus fe
  in mkFieldElement result p

-- ============================================================
-- Lagrange Interpolation
-- ============================================================

-- | Lagrange interpolation: given points [(x_i, y_i)], produce
-- the unique polynomial of degree <= n-1 passing through all points.
--
-- Works over GF(p).
lagrangeInterpolate :: Integer -> Int -> [(Integer, Integer)] -> Either AlgebircError BoundedPoly
lagrangeInterpolate _ _ [] = Left (GenericError "Empty point set")
lagrangeInterpolate fm md points
  | length points - 1 > md = Left (DegreeOverflow (length points - 1) md)
  | otherwise =
      let n = length points
          -- L_i(x) = prod_{j /= i} (x - x_j) / (x_i - x_j)
          basis i =
            let (xi, yi) = points !! i
                others = [ xj | (j, (xj, _)) <- zip [0..] points, j /= i ]
                -- Numerator: product of (x - xj) polynomials
                numPolys = map (\xj -> linearPoly fm md 1 (negate xj)) others
                -- Denominator: product of (xi - xj) scalars
                denom = product [ (xi - xj) `mod'` fm | xj <- others ]
            in case foldM polyMul (constPoly fm md 1) numPolys of
                 Left err -> Left err
                 Right numPoly ->
                   case modInverse denom fm of
                     Nothing -> Left (InverseNotFound $ "denom=" ++ show denom)
                     Just denomInv ->
                       Right (polyScale (yi * denomInv `mod'` fm) numPoly)
          foldM _ z []     = Right z
          foldM f z (x:xs) = do r <- f z x; foldM f r xs
      in case mapM basis [0..n-1] of
           Left err -> Left err
           Right basisPolys -> Right $ foldl polyAdd (zeroPoly fm md) basisPolys

-- | Modular remainder that always returns non-negative.
mod' :: Integer -> Integer -> Integer
mod' a m
  | m == 0    = a
  | otherwise = ((a `mod` m) + m) `mod` m

-- | Modular inverse using extended GCD.
modInverse :: Integer -> Integer -> Maybe Integer
modInverse _ 0 = Just 1  -- no modulus
modInverse a m =
  let (g, x, _) = extGcd (a `mod'` m) m
  in if g == 1
     then Just ((x `mod` m + m) `mod` m)
     else Nothing
