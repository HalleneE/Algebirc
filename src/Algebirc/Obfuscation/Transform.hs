-- |
-- Module      : Algebirc.Obfuscation.Transform
-- Description : Nonlinear algebraic transformations for obfuscation
-- License     : MIT
--
-- Provides composable algebraic transforms that obfuscate encoded
-- data while preserving recoverability (with the correct key).
--
-- = Transform Types
--
-- 1. __Affine__: @x → a*x + b (mod p)@, invertible when gcd(a,p)=1
-- 2. __Polynomial substitution__: evaluate secret polynomial at each element
-- 3. __Permutation__: byte-level shuffle via group action
-- 4. __Composite__: sequential composition of multiple transforms

module Algebirc.Obfuscation.Transform
  ( -- * Transform Application
    applyTransform
  , invertTransform
    -- * Transform Construction
  , mkAffineTransform
  , mkPolyTransform
  , mkPermTransform
  , mkCompositeTransform
    -- * Pipeline
  , applyPipeline
  , invertPipeline
    -- * Obfuscation API
  , obfuscateBlock
  , deobfuscateBlock
  ) where

import Algebirc.Core.Types
import Algebirc.Core.FiniteField
import Algebirc.Core.Polynomial
import Algebirc.Core.Group
import Algebirc.Obfuscation.Encoder (EncodedBlock(..))

-- ============================================================
-- Transform Construction
-- ============================================================

-- | Create an affine transform: x → a*x + b (mod p).
-- Invertible when gcd(a, p) = 1 (always for prime p, a ≠ 0).
mkAffineTransform :: Integer -> Integer -> Transform
mkAffineTransform a b = Transform
  { transformTag    = AffineTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Just a
  , transformB      = Just b
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

-- | Create a polynomial substitution transform.
-- Applies f(x) to each coefficient.
mkPolyTransform :: BoundedPoly -> Transform
mkPolyTransform poly = Transform
  { transformTag    = PolynomialTransform
  , transformPoly   = Just poly
  , transformPerm   = Nothing
  , transformA      = Nothing
  , transformB      = Nothing
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

-- | Create a permutation transform for coefficient reordering.
mkPermTransform :: Permutation -> Transform
mkPermTransform perm = Transform
  { transformTag    = PermutationTransform
  , transformPoly   = Nothing
  , transformPerm   = Just perm
  , transformA      = Nothing
  , transformB      = Nothing
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

-- | Compose multiple transforms into a pipeline.
mkCompositeTransform :: [Transform] -> Transform
mkCompositeTransform ts = Transform
  { transformTag    = CompositeTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Nothing
  , transformB      = Nothing
  , transformSubs   = ts
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
-- Transform Application
-- ============================================================

-- | Apply a transform to polynomial coefficients.
-- The transform operates on each coefficient independently
-- (substitution cipher over GF(p)).
applyTransform :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyTransform cfg t poly = case transformTag t of
  AffineTransform      -> applyAffine cfg t poly
  PolynomialTransform  -> applyPolySub cfg t poly
  PermutationTransform -> applyPermutation t poly
  CompositeTransform   -> applyPipeline cfg (transformSubs t) poly
  SBoxTransform        -> Left (GenericError "S-box transform: use NonlinearTransform module")
  FeistelTransform     -> Left (GenericError "Feistel transform: use NonlinearTransform module")
  PowerMapTransform    -> Left (GenericError "Power map transform: use NonlinearTransform module")
  ARXDiffusionTransform -> Left (GenericError "ARX diffusion: use NonlinearTransform module")

-- | Invert a transform (recover original data).
invertTransform :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertTransform cfg t poly = case transformTag t of
  AffineTransform      -> invertAffine cfg t poly
  PolynomialTransform  -> Left (GenericError "Polynomial transform inversion requires lookup table")
  PermutationTransform -> invertPermutation t poly
  CompositeTransform   -> invertPipeline cfg (transformSubs t) poly
  SBoxTransform        -> Left (GenericError "S-box inversion: use NonlinearTransform module")
  FeistelTransform     -> Left (GenericError "Feistel inversion: use NonlinearTransform module")
  PowerMapTransform    -> Left (GenericError "Power map inversion: use NonlinearTransform module")
  ARXDiffusionTransform -> Left (GenericError "ARX inversion: use NonlinearTransform module")

-- ============================================================
-- Affine Transform: x → a*x + b (mod p)
-- ============================================================
--
-- CRITICAL: We must operate on ALL coefficient positions [0..maxPos],
-- not just existing terms. Zero is a valid data value (represents a byte),
-- so canonical normalization (which strips zero terms) would lose data.

applyAffine :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyAffine cfg t poly =
  case (transformA t, transformB t) of
    (Just a, Just b) ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          -- Fill ALL positions up to degree cap — prevents data loss
          -- when chaining transforms (degree can't shrink between steps)
          coeffAt i = getCoeffAt i poly
          newTerms = [ Term (((a * coeffAt i) + b) `mod` p) i
                     | i <- [0 .. maxDeg] ]
      in Right $ mkBoundedPoly p maxDeg newTerms
    _ -> Left (GenericError "Affine transform missing a or b")

invertAffine :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertAffine cfg t poly =
  case (transformA t, transformB t) of
    (Just a, Just b) ->
      let p = cfgFieldPrime cfg
      in case ffInv (mkFieldElement a p) of
           Left err -> Left err
           Right aInv ->
             let aInvVal = feValue aInv
                 maxDeg = polyMaxDegree poly
                 coeffAt i = getCoeffAt i poly
                 newTerms = [ Term ((aInvVal * (coeffAt i - b + p)) `mod` p) i
                            | i <- [0 .. maxDeg] ]
             in Right $ mkBoundedPoly p maxDeg newTerms
    _ -> Left (GenericError "Affine transform missing a or b")

-- ============================================================
-- Polynomial Substitution: c → f(c) (mod p)
-- ============================================================

applyPolySub :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPolySub cfg t (BoundedPoly terms maxDeg _) =
  case transformPoly t of
    Just subPoly ->
      let p = cfgFieldPrime cfg
          transformCoeff (Term c e) =
            Term (polyEval subPoly c) e
      in Right $ mkBoundedPoly p maxDeg (map transformCoeff terms)
    Nothing -> Left (GenericError "Polynomial transform missing polynomial")

-- ============================================================
-- Permutation: reorder coefficients
-- ============================================================

applyPermutation :: Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPermutation t (BoundedPoly terms maxDeg fm) =
  case transformPerm t of
    Just perm ->
      let n = permSize perm
          -- Remap exponents: term at position i goes to position σ(i)
          transformTerm (Term c e) =
            if e < n
            then Term c (applyPerm perm e)
            else Term c e  -- leave terms beyond permutation size untouched
      in Right $ mkBoundedPoly fm maxDeg (map transformTerm terms)
    Nothing -> Left (GenericError "Permutation transform missing permutation")

invertPermutation :: Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPermutation t poly =
  case transformPerm t of
    Just perm ->
      let invT = t { transformPerm = Just (invertPerm perm) }
      in applyPermutation invT poly
    Nothing -> Left (GenericError "Permutation transform missing permutation")

-- ============================================================
-- Pipeline (Composite)
-- ============================================================

-- | Apply transforms in order: t1, then t2, then t3, ...
applyPipeline :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPipeline _ [] poly = Right poly
applyPipeline cfg (t:ts) poly = do
  result <- applyTransform cfg t poly
  applyPipeline cfg ts result

-- | Invert pipeline in reverse order.
invertPipeline :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPipeline _ [] poly = Right poly
invertPipeline cfg ts poly = do
  let reversed = reverse ts
  foldlEither (\p t -> invertTransform cfg t p) poly reversed

foldlEither :: (a -> b -> Either e a) -> a -> [b] -> Either e a
foldlEither _ acc [] = Right acc
foldlEither f acc (x:xs) = do
  acc' <- f acc x
  foldlEither f acc' xs

-- ============================================================
-- High-Level Obfuscation API
-- ============================================================

-- | Obfuscate an encoded block using a transform pipeline.
obfuscateBlock :: ObfuscationConfig -> [Transform] -> EncodedBlock -> Either AlgebircError EncodedBlock
obfuscateBlock cfg transforms block = do
  newPoly <- applyPipeline cfg transforms (ebPoly block)
  return block { ebPoly = newPoly }

-- | Deobfuscate a block (reverse the transforms).
deobfuscateBlock :: ObfuscationConfig -> [Transform] -> EncodedBlock -> Either AlgebircError EncodedBlock
deobfuscateBlock cfg transforms block = do
  newPoly <- invertPipeline cfg transforms (ebPoly block)
  return block { ebPoly = newPoly }
