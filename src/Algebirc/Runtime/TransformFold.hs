-- |
-- Module      : Algebirc.Runtime.TransformFold
-- Description : Compile-time transform composition for pure algebraic pipelines
-- License     : MIT
--
-- = Strategy
--
-- Algebraically compose multiple transforms into fewer (or one) equivalent
-- transform. This eliminates intermediate states at runtime.
--
-- Folding rules:
--   Affine ∘ Affine  → single Affine
--   Poly  ∘ Poly     → result of polynomial composition
--   Identity removal → dead transform elimination

module Algebirc.Runtime.TransformFold
  ( -- * Pipeline Folding
    foldPipeline
  , foldAffineChain
  , composePolynomials
  , eliminateIdentity
    -- * Analysis
  , countMultiplications
  , foldingReport
  ) where

import Algebirc.Core.Types

-- ============================================================
-- Pipeline Optimizer
-- ============================================================

-- | Optimize a transform pipeline by folding composable transforms.
foldPipeline :: Integer -> [Transform] -> [Transform]
foldPipeline p = fixpoint (foldPass p)
  where
    fixpoint f xs =
      let xs' = f xs
      in if length xs' == length xs then xs' else fixpoint f xs'

-- | Single fold pass: apply all folding rules once.
foldPass :: Integer -> [Transform] -> [Transform]
foldPass p = foldAdjacentAffines p . eliminateIdentity

-- ============================================================
-- Rule 1: Identity Elimination
-- ============================================================

-- | Remove transforms that are identity operations.
eliminateIdentity :: [Transform] -> [Transform]
eliminateIdentity = filter (not . isIdentityTransform)

-- | Check if a transform is an identity operation.
isIdentityTransform :: Transform -> Bool
isIdentityTransform t = case transformTag t of
  AffineTransform ->
    case (transformA t, transformB t) of
      (Just 1, Just 0) -> True
      _                -> False
  PolynomialTransform ->
    case transformPoly t of
      Just poly -> isIdentityPoly poly
      Nothing   -> False
  CompositeTransform ->
    null (transformSubs t)
  _ -> False

-- | Check if a polynomial is the identity f(x) = x.
isIdentityPoly :: BoundedPoly -> Bool
isIdentityPoly poly =
  let deg = polyMaxDegree poly
  in deg == 1
     && getCoeffAt 0 poly == 0
     && getCoeffAt 1 poly == 1

-- ============================================================
-- Rule 2: Affine Chain Folding
-- ============================================================

-- | Fold all adjacent affine transforms in a pipeline.
foldAdjacentAffines :: Integer -> [Transform] -> [Transform]
foldAdjacentAffines _ [] = []
foldAdjacentAffines p (t1:t2:rest)
  | isAffine t1 && isAffine t2 =
      foldAdjacentAffines p (foldTwoAffines p t1 t2 : rest)
  | otherwise = t1 : foldAdjacentAffines p (t2:rest)
foldAdjacentAffines _ [t] = [t]

-- | Fold exactly two affine transforms into one.
foldTwoAffines :: Integer -> Transform -> Transform -> Transform
foldTwoAffines p t1 t2 =
  let a1 = maybe 1 id (transformA t1)
      b1 = maybe 0 id (transformB t1)
      a2 = maybe 1 id (transformA t2)
      b2 = maybe 0 id (transformB t2)
      a' = (a2 * a1) `mod` p
      b' = (a2 * b1 + b2) `mod` p
  in t1 { transformA = Just a'
        , transformB = Just b'
        }

-- | Fold an arbitrary chain of affine transforms into one.
foldAffineChain :: Integer -> [Transform] -> Transform
foldAffineChain p ts =
  case filter isAffine ts of
    []     -> mkIdentityAffine
    (x:xs) -> foldl (foldTwoAffines p) x xs

-- | Identity affine transform.
mkIdentityAffine :: Transform
mkIdentityAffine = Transform
  { transformTag    = AffineTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Just 1
  , transformB      = Just 0
  , transformSubs   = []
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  , transformCurve  = Nothing
  , transformHyper  = Nothing
  , transformIgusa  = Nothing
  , transformIsogeny = Nothing
  }

-- ============================================================
-- Rule 3: Polynomial Composition
-- ============================================================

-- | Compose two polynomials: h(x) = f(g(x)) mod p.
composePolynomials :: Integer -> BoundedPoly -> BoundedPoly -> BoundedPoly
composePolynomials p f g =
  let degF = polyMaxDegree f
      degG = polyMaxDegree g
      resultDeg = degF * degG
      gPowers = take (degF + 1) $ iterate (polyMulMod p resultDeg g) (constPoly p resultDeg 1)
      terms = [ scalarMulPoly p (getCoeffAt k f) (gPowers !! k) | k <- [0..degF] ]
  in foldl (polyAddMod p resultDeg) (constPoly p resultDeg 0) terms

-- | Modular polynomial multiplication.
polyMulMod :: Integer -> Int -> BoundedPoly -> BoundedPoly -> BoundedPoly
polyMulMod p maxD a b =
  let degA = polyMaxDegree a
      degB = polyMaxDegree b
      terms = [ Term (((getCoeffAt i a) * (getCoeffAt j b)) `mod` p) (i + j)
              | i <- [0..degA], j <- [0..degB]
              , i + j <= maxD ]
      combined = foldl (\acc (Term c d) ->
        let old = getCoeffAt d acc
            ts  = [Term ((old + c) `mod` p) d]
        in mkBoundedPoly p maxD (ts ++ [Term (getCoeffAt k acc) k | k <- [0..maxD], k /= d])
        ) (constPoly p maxD 0) terms
  in combined

-- | Modular polynomial addition.
polyAddMod :: Integer -> Int -> BoundedPoly -> BoundedPoly -> BoundedPoly
polyAddMod p maxD a b =
  let terms = [ Term ((getCoeffAt k a + getCoeffAt k b) `mod` p) k | k <- [0..maxD] ]
  in mkBoundedPoly p maxD terms

-- | Scalar multiplication of polynomial.
scalarMulPoly :: Integer -> Integer -> BoundedPoly -> BoundedPoly
scalarMulPoly p s poly =
  let d = polyMaxDegree poly
  in mkBoundedPoly p d [ Term ((s * getCoeffAt k poly) `mod` p) k | k <- [0..d] ]

-- | Constant polynomial.
constPoly :: Integer -> Int -> Integer -> BoundedPoly
constPoly p d c = mkBoundedPoly p d [Term (c `mod` p) 0]

-- ============================================================
-- Analysis
-- ============================================================

isAffine :: Transform -> Bool
isAffine t = transformTag t == AffineTransform

isPoly :: Transform -> Bool
isPoly t = transformTag t == PolynomialTransform

-- | Count multiplications in a pipeline.
countMultiplications :: [Transform] -> Int
countMultiplications = sum . map countMuls
  where
    countMuls t = case transformTag t of
      AffineTransform      -> 1
      PolynomialTransform  ->
        case transformPoly t of
          Just poly -> polyMaxDegree poly
          Nothing   -> 0
      PowerMapTransform    ->
        case transformExp t of
          Just e  -> ceiling (logBase 2.0 (fromIntegral e) :: Double)
          Nothing -> 0
      CompositeTransform   -> countMultiplications (transformSubs t)
      ARXDiffusionTransform -> transformRounds t * 3
      _                    -> 0

-- | Generate a report comparing before/after folding.
foldingReport :: Integer -> [Transform] -> String
foldingReport p ts =
  let before = countMultiplications ts
      folded = foldPipeline p ts
      after  = countMultiplications folded
      reduction = if before > 0
                  then (fromIntegral (before - after) / fromIntegral before * 100 :: Double)
                  else 0
  in unlines
    [ "=== Transform Folding Report ==="
    , "Before: " ++ show (length ts) ++ " transforms, " ++ show before ++ " muls"
    , "After:  " ++ show (length folded) ++ " transforms, " ++ show after ++ " muls"
    , "Reduction: " ++ show (round reduction :: Int) ++ "%"
    , "Tags before: " ++ show (map transformTag ts)
    , "Tags after:  " ++ show (map transformTag folded)
    ]
