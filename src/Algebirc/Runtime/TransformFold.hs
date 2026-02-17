-- |
-- Module      : Algebirc.Runtime.TransformFold
-- Description : Compile-time transform composition — eliminate intermediate states
-- License     : MIT
--
-- = Strategy
--
-- Algebraically compose multiple transforms into fewer (or one) equivalent
-- transform.  After folding, intermediate states DON'T EXIST at runtime.
-- An attacker tracing execution sees only the folded result.
--
-- Folding rules:
--   Affine ∘ Affine  → single Affine
--   Poly  ∘ Poly     → resultant polynomial
--   SBox  + Affine   → fused lookup table
--   Identity removal → dead transform elimination

module Algebirc.Runtime.TransformFold
  ( -- * Pipeline Folding
    foldPipeline
  , foldAffineChain
  , composePolynomials
  , fuseSBoxAffine
  , eliminateIdentity
    -- * Analysis
  , countMultiplications
  , foldingReport
  ) where

import Algebirc.Core.Types
import qualified Data.Vector.Unboxed as VU

-- ============================================================
-- Pipeline Optimizer
-- ============================================================

-- | Optimize a transform pipeline by folding composable transforms.
-- Apply rules greedily left-to-right:
--   1. Eliminate identity transforms
--   2. Fold adjacent affines
--   3. Fuse SBox + Affine pairs
--   4. Fold adjacent polynomials
-- Iterate until no more folding possible (fixed-point).
foldPipeline :: Integer -> [Transform] -> [Transform]
foldPipeline p = fixpoint (foldPass p)
  where
    fixpoint f xs =
      let xs' = f xs
      in if length xs' == length xs then xs' else fixpoint f xs'

-- | Single fold pass: apply all folding rules once.
foldPass :: Integer -> [Transform] -> [Transform]
foldPass p = foldSBoxAffine p . foldAdjacentAffines p . eliminateIdentity

-- ============================================================
-- Rule 1: Identity Elimination
-- ============================================================

-- | Remove transforms that are identity operations.
-- Affine with a=1, b=0 → identity
-- Polynomial = [0, 1] (i.e. f(x) = x) → identity
-- Composite with empty subs → identity
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
-- (a₂, b₂) ∘ (a₁, b₁) ≡ (a₂·a₁, a₂·b₁ + b₂)  mod p
foldAdjacentAffines :: Integer -> [Transform] -> [Transform]
foldAdjacentAffines _ [] = []
foldAdjacentAffines p (t1:t2:rest)
  | isAffine t1 && isAffine t2 =
      -- Fold and continue (might fold with next too)
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
      -- (a₂·(a₁·x + b₁) + b₂) = (a₂·a₁)·x + (a₂·b₁ + b₂)
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

-- | Identity affine transform: x → 1·x + 0.
mkIdentityAffine :: Transform
mkIdentityAffine = Transform
  { transformTag    = AffineTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Just 1
  , transformB      = Just 0
  , transformSubs   = []
  , transformSBox   = Nothing
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  , transformCurve  = Nothing
  , transformHyper  = Nothing
  , transformIgusa  = Nothing
  , transformIsogeny = Nothing
  }

-- ============================================================
-- Rule 3: SBox + Affine Fusion
-- ============================================================

-- | Fuse adjacent SBox + Affine into a single modified SBox.
-- S'(x) = a · S(x) + b  — pre-computed as new lookup table.
foldSBoxAffine :: Integer -> [Transform] -> [Transform]
foldSBoxAffine _ [] = []
foldSBoxAffine p (t1:t2:rest)
  | isSBox t1 && isAffine t2 =
      case (transformSBox t1, transformA t2, transformB t2) of
        (Just sb, Just a, Just b) ->
          let fused = fuseSBoxAffine sb a b p
              fusedT = t1 { transformSBox = Just fused }
          in foldSBoxAffine p (fusedT : rest)
        _ -> t1 : foldSBoxAffine p (t2:rest)
  | otherwise = t1 : foldSBoxAffine p (t2:rest)
foldSBoxAffine _ [t] = [t]

-- | Fuse an SBox with a post-affine: S'(x) = a · S(x) + b.
-- Creates a new SBox with the affine baked in.
fuseSBoxAffine :: SBox -> Integer -> Integer -> Integer -> SBox
fuseSBoxAffine sb a b p =
  let n = fromIntegral p
      -- Apply affine to each SBox output
      newFwd = VU.generate n (\i ->
        let sOut = fromIntegral $ sboxFwd sb VU.! i
            fused = (a * sOut + b) `mod` p
        in fromIntegral fused)
      -- Compute inverse of new table
      newInv = VU.replicate n 0
                 VU.// [(newFwd VU.! i, i) | i <- [0..n-1]]
  in SBox newFwd newInv p

-- ============================================================
-- Rule 4: Polynomial Composition
-- ============================================================

-- | Compose two polynomials: h(x) = f(g(x)) mod p.
-- Result degree = deg(f) * deg(g).
composePolynomials :: Integer -> BoundedPoly -> BoundedPoly -> BoundedPoly
composePolynomials p f g =
  let degF = polyMaxDegree f
      degG = polyMaxDegree g
      resultDeg = degF * degG

      -- Compute g(x)^k for k = 0, 1, ..., degF
      -- g^0 = [1, 0, 0, ...]
      -- g^k = g^(k-1) * g
      gPowers = take (degF + 1) $ iterate (polyMulMod p resultDeg g) (constPoly p resultDeg 1)

      -- h(x) = sum_{k=0}^{degF} f_k * g(x)^k
      terms = [ scalarMulPoly p (getCoeffAt k f) (gPowers !! k)
              | k <- [0..degF] ]

      result = foldl (polyAddMod p resultDeg) (constPoly p resultDeg 0) terms
  in result

-- | Multiply two polynomials mod p, capped at degree d.
polyMulMod :: Integer -> Int -> BoundedPoly -> BoundedPoly -> BoundedPoly
polyMulMod p maxD a b =
  let degA = polyMaxDegree a
      degB = polyMaxDegree b
      terms = [ Term (((getCoeffAt i a) * (getCoeffAt j b)) `mod` p) (i + j)
              | i <- [0..degA], j <- [0..degB]
              , i + j <= maxD ]
      -- Combine terms with same degree
      combined = foldl (\acc (Term c d) ->
        let old = getCoeffAt d acc
            new = (old + c) `mod` p
            ts  = [Term new d]
        in mkBoundedPoly p maxD (ts ++ [Term (getCoeffAt k acc) k | k <- [0..maxD], k /= d])
        ) (constPoly p maxD 0) terms
  in combined

-- | Add two polynomials mod p.
polyAddMod :: Integer -> Int -> BoundedPoly -> BoundedPoly -> BoundedPoly
polyAddMod p maxD a b =
  let terms = [ Term ((getCoeffAt k a + getCoeffAt k b) `mod` p) k
              | k <- [0..maxD] ]
  in mkBoundedPoly p maxD terms

-- | Scalar multiplication of polynomial.
scalarMulPoly :: Integer -> Integer -> BoundedPoly -> BoundedPoly
scalarMulPoly p s poly =
  let d = polyMaxDegree poly
      terms = [ Term ((s * getCoeffAt k poly) `mod` p) k | k <- [0..d] ]
  in mkBoundedPoly p d terms

-- | Constant polynomial.
constPoly :: Integer -> Int -> Integer -> BoundedPoly
constPoly p d c = mkBoundedPoly p d [Term (c `mod` p) 0]

-- ============================================================
-- Analysis
-- ============================================================

-- | Classify a transform.
isAffine :: Transform -> Bool
isAffine t = transformTag t == AffineTransform

isSBox :: Transform -> Bool
isSBox t = transformTag t == SBoxTransform

isPoly :: Transform -> Bool
isPoly t = transformTag t == PolynomialTransform

-- | Count multiplications in a pipeline (for cost estimation).
countMultiplications :: [Transform] -> Int
countMultiplications = sum . map countMuls
  where
    countMuls t = case transformTag t of
      AffineTransform      -> 1     -- a*x
      PolynomialTransform  ->
        case transformPoly t of
          Just poly -> polyMaxDegree poly  -- Horner: deg multiplications
          Nothing   -> 0
      SBoxTransform        -> 0     -- lookup, no mul
      FeistelTransform     -> transformRounds t * 2  -- SBox + quad per round
      PowerMapTransform    ->
        case transformExp t of
          Just e  -> ceiling (logBase 2.0 (fromIntegral e) :: Double)  -- square-and-multiply
          Nothing -> 0
      CompositeTransform   -> countMultiplications (transformSubs t)
      ARXDiffusionTransform -> transformRounds t * 3  -- 3 passes per round
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
