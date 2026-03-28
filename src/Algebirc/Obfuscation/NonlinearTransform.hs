-- |
-- Module      : Algebirc.Obfuscation.NonlinearTransform
-- Description : Pure Algebraic Nonlinear Transforms — Power Map, MDS Matrix
-- License     : MIT
--
-- = Algebraic Purism (Phase 4)
--
-- 1. __Power Map (P1)__: x → x^e mod p (bijective substitutions).
-- 2. __MDS Matrix (P2)__: Cauchy MDS diffusion layers (linear mixing).
-- 3. __Algebraic SPN__: Alternative layers for formal verification.

module Algebirc.Obfuscation.NonlinearTransform
  ( -- * Transform Constructors
    mkPowerMapTransform
  , mkAffineTransform
    -- * Application / Inversion
  , applyNonlinear
  , invertNonlinear
    -- * Key-Based Pipeline
  , generatePipeline
  , extractStructure
    -- * Pure Algebraic Primitives
  , powerMapPure
  , mdsDiffusePure
  , mdsUndiffusePure
  ) where

import Algebirc.Core.Types
import Algebirc.Core.Matrix (mkCauchyMatrix, matApplyField, matInverseField)
import Algebirc.Geometry.EllipticCurve (modPow)

-- ============================================================
-- Transform Constructors
-- ============================================================

-- | Create pure affine transform.
mkAffineTransform :: Integer -> Integer -> Transform
mkAffineTransform a b =
  Transform
    { transformTag    = AffineTransform
    , transformPoly   = Nothing
    , transformPerm   = Nothing
    , transformA      = Just a
    , transformB      = Just b
    , transformSubs   = []
    , transformExp    = Nothing
    , transformRounds = 0
    , transformKey    = Nothing
    , transformCurve  = Nothing
    , transformHyper  = Nothing
    , transformIgusa  = Nothing
    , transformIsogeny = Nothing
    }

-- | Create power map transform: x → x^e mod p.
mkPowerMapTransform :: Integer -> Integer -> Either AlgebircError Transform
mkPowerMapTransform e p
  | gcd e (p - 1) /= 1 =
      Left (GenericError $ "Power map exponent e=" ++ show e
                        ++ " not coprime with p-1=" ++ show (p-1))
  | otherwise = Right Transform
      { transformTag    = PowerMapTransform
      , transformPoly   = Nothing
      , transformPerm   = Nothing
      , transformA      = Nothing
      , transformB      = Nothing
      , transformSubs   = []
      , transformExp    = Just e
      , transformRounds = 0
      , transformKey    = Nothing
      , transformCurve  = Nothing
      , transformHyper  = Nothing
      , transformIgusa  = Nothing
      , transformIsogeny = Nothing
      }

-- ============================================================
-- Pure Algebraic Primitives
-- ============================================================

-- | Pure Power Map Layer [P1].
powerMapPure :: Integer -> Integer -> Integer -> Integer
powerMapPure p d x = if x == 0 then 0 else modPow x d p

-- | Pure MDS Diffusion Layer [P2].
mdsDiffusePure :: Integer -> Int -> [Integer] -> [Integer]
mdsDiffusePure p n coeffs =
  let m = mkCauchyMatrix p n
  in matApplyField p m coeffs

-- | Inverse MDS Diffusion.
mdsUndiffusePure :: Integer -> Int -> [Integer] -> [Integer]
mdsUndiffusePure p n coeffs =
  let m = mkCauchyMatrix p n
  in case matInverseField p m of
       Right mInv -> matApplyField p mInv coeffs
       Left _     -> coeffs -- Fallback for singular matrix (should not happen for Cauchy)

-- ============================================================
-- Apply / Invert Nonlinear Transforms
-- ============================================================

applyNonlinear :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyNonlinear cfg t poly =
  if cfgEnableAnalysis cfg
  then Right poly -- Bypass for degree analysis
  else case transformTag t of
    PowerMapTransform -> applyPowerMap cfg t poly
    ARXDiffusionTransform -> applyMDSStage cfg t poly
    _                -> Left (GenericError "applyNonlinear: invalid pure transform tag")

invertNonlinear :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertNonlinear cfg t poly =
  if cfgEnableAnalysis cfg
  then Right poly
  else case transformTag t of
    PowerMapTransform -> invertPowerMap cfg t poly
    ARXDiffusionTransform -> invertMDSStage cfg t poly
    _                -> Left (GenericError "invertNonlinear: invalid pure transform tag")

-- | Apply MDS Diffusion Stage.
applyMDSStage :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyMDSStage cfg t poly =
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree poly
      coeffs = [ getCoeffAt i poly | i <- [0..maxDeg] ]
      n = length coeffs
      finalCoeffs = mdsDiffusePure p n coeffs
      newTerms = [ Term (finalCoeffs !! i) i | i <- [0..maxDeg] ]
  in Right $ mkBoundedPoly p maxDeg newTerms

-- | Invert MDS Diffusion Stage.
invertMDSStage :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertMDSStage cfg t poly =
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree poly
      coeffs = [ getCoeffAt i poly | i <- [0..maxDeg] ]
      n = length coeffs
      finalCoeffs = mdsUndiffusePure p n coeffs
      newTerms = [ Term (finalCoeffs !! i) i | i <- [0..maxDeg] ]
  in Right $ mkBoundedPoly p maxDeg newTerms

-- | Apply Power Map: x → x^e mod p.
applyPowerMap :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPowerMap cfg t poly =
  case transformExp t of
    Nothing -> Left (GenericError "Power map missing exponent")
    Just e ->
      let p = cfgFieldPrime cfg
          maxDeg = polyMaxDegree poly
          newTerms = [ Term (powerMapPure p e (getCoeffAt i poly)) i | i <- [0..maxDeg] ]
      in Right $ mkBoundedPoly p maxDeg newTerms

-- | Invert Power Map: x → x^{e⁻¹} mod p.
invertPowerMap :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPowerMap cfg t poly =
  case transformExp t of
    Nothing -> Left (GenericError "Power map missing exponent")
    Just e ->
      let p = cfgFieldPrime cfg
          eInv = modInverse e (p - 1)
      in case eInv of
           Nothing -> Left (GenericError "Cannot invert power map (gcd(e,p-1) != 1)")
           Just ei -> applyPowerMap cfg (t { transformExp = Just ei }) poly

-- ============================================================
-- Pipeline Generation
-- ============================================================

-- | Generate a hardened pipeline from secret key.
-- Structure: [Affine → PowerMap → MDS → PowerMap → MDS]
generatePipeline :: ObfuscationConfig -> SecretKey -> Either AlgebircError [Transform]
generatePipeline cfg key = do
  let p = cfgFieldPrime cfg
  let e = findCoprime (skPowerExp key) (p - 1)
  pmTransform <- mkPowerMapTransform e p

  -- 2. Pure Affine Transform
  let seed = skSeed key
      a = ((seed * 31 + 17) `mod` (p - 1)) + 1
      b = 0
      affine = mkAffineTransform a b

  -- 3. Pure MDS Diffusion (P2)
  let mds = Transform
        { transformTag    = ARXDiffusionTransform
        , transformPoly   = Nothing
        , transformPerm   = Nothing
        , transformA      = Nothing
        , transformB      = Nothing
        , transformSubs   = []
        , transformExp    = Nothing
        , transformRounds = 1
        , transformKey    = Just key
        , transformCurve  = Nothing
        , transformHyper  = Nothing
        , transformIgusa  = Nothing
        , transformIsogeny = Nothing
        }

  -- Final Pure Pipeline
  return [affine, pmTransform, mds, pmTransform, mds]

-- ============================================================
-- Pure Utilities
-- ============================================================

-- | Find closest coprime to target with n.
findCoprime :: Integer -> Integer -> Integer
findCoprime target n =
  let candidates = [target, target + 1 .. target + 100] ++ [target - 1, target - 2 .. max 2 (target - 100)]
  in case filter (\c -> c > 1 && gcd c n == 1) candidates of
       (c:_) -> c
       []    -> 3

-- | Modular inverse via extended GCD.
modInverse :: Integer -> Integer -> Maybe Integer
modInverse a m =
  let (g, x, _) = extGCD a m
  in if g == 1 then Just ((x `mod` m + m) `mod` m)
     else Nothing
  where
    extGCD 0 b = (b, 0, 1)
    extGCD a' b' =
      let (g', x', y') = extGCD (b' `mod` a') a'
      in (g', y' - (b' `div` a') * x', x')

-- | Extract public pipeline structure.
extractStructure :: ObfuscationConfig -> [Transform] -> PipelineStructure
extractStructure cfg transforms = PipelineStructure
  { psTransformTags = map transformTag transforms
  , psDegree        = cfgMaxDegree cfg
  , psFieldPrime    = cfgFieldPrime cfg
  , psLayerCount    = length transforms
  }
