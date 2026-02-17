-- |
-- Module      : Algebirc.Obfuscation.DegreeControl
-- Description : Degree budget tracking and explosion prevention
-- License     : MIT
--
-- = Design
--
-- Without degree control, nonlinear transforms cause:
-- d → d² → d⁴ → d⁸ → chaos
--
-- This module provides:
-- 1. Predictive bounds: compute max degree BEFORE applying transform
-- 2. Budget tracking: cumulative degree usage per layer
-- 3. Rejection: refuse composition that exceeds budget

module Algebirc.Obfuscation.DegreeControl
  ( -- * Degree Budget
    DegreeBudget(..)
  , computeDegreeBudget
    -- * Predictive Bounds
  , maxDegreeAfterTransform
    -- * Validation
  , validatePipeline
  , validateTransform
  ) where

import Algebirc.Core.Types

-- | Degree budget tracking across pipeline layers.
data DegreeBudget = DegreeBudget
  { dbPerLayer   :: ![(String, Int, Int)]  -- ^ (tag, degree_in, degree_out) per layer
  , dbMaxReached :: !Int                   -- ^ Maximum degree reached at any point
  , dbBudget     :: !Int                   -- ^ Allowed maximum (from config)
  , dbExceeded   :: !Bool                  -- ^ True if budget was violated
  } deriving (Show, Eq)

-- | Compute degree budget for a pipeline without executing it.
-- Pure prediction — O(n) in pipeline length.
computeDegreeBudget :: ObfuscationConfig -> [Transform] -> Int -> DegreeBudget
computeDegreeBudget cfg transforms inputDeg =
  let budget = cfgMaxDegree cfg
      layers = scanLayers inputDeg transforms
      maxReached = maximum $ inputDeg : map (\(_, _, d) -> d) layers
      exceeded = maxReached > budget
  in DegreeBudget
    { dbPerLayer   = layers
    , dbMaxReached = maxReached
    , dbBudget     = budget
    , dbExceeded   = exceeded
    }

-- | Scan layers: compute degree at each step.
scanLayers :: Int -> [Transform] -> [(String, Int, Int)]
scanLayers _ [] = []
scanLayers degIn (t:ts) =
  let degOut = maxDegreeAfterTransform t degIn
      tag = tagName (transformTag t)
      rest = scanLayers degOut ts
  in (tag, degIn, degOut) : rest

-- | Predict maximum degree after applying a transform.
-- Conservative upper bound — may overestimate for safety.
maxDegreeAfterTransform :: Transform -> Int -> Int
maxDegreeAfterTransform t degIn = case transformTag t of
  -- Affine: x → ax + b — degree preserved
  AffineTransform      -> degIn

  -- Polynomial sub: c → f(c) — degree preserved (pointwise on coefficients)
  PolynomialTransform  -> degIn

  -- Permutation: position shuffling — degree preserved
  PermutationTransform -> degIn

  -- Composite: fold through sub-transforms
  CompositeTransform   ->
    foldl (\d sub -> maxDegreeAfterTransform sub d) degIn (transformSubs t)

  -- S-Box: pointwise lookup — degree preserved
  SBoxTransform        -> degIn

  -- Feistel: L/R mixing — degree preserved (coefficient-level, not poly composition)
  FeistelTransform     -> degIn

  -- Power map: x → x^e — operates on coefficients, not polynomial composition
  -- Degree preserved because we apply to coefficients independently
  PowerMapTransform    -> degIn

  -- ARX diffusion: full-width mixing — degree preserved
  ARXDiffusionTransform -> degIn

-- | Validate that a pipeline won't exceed degree budget.
-- Returns Nothing if OK, Just error if violated.
validatePipeline :: ObfuscationConfig -> [Transform] -> Int -> Maybe String
validatePipeline cfg transforms inputDeg =
  let budget = computeDegreeBudget cfg transforms inputDeg
  in if dbExceeded budget
     then Just $ "Degree budget exceeded: max reached "
              ++ show (dbMaxReached budget)
              ++ " > budget " ++ show (dbBudget budget)
              ++ " at layers: " ++ show (map (\(t, _, d) -> (t, d))
                                      (filter (\(_, _, d) -> d > dbBudget budget) (dbPerLayer budget)))
     else Nothing

-- | Validate a single transform won't exceed degree.
validateTransform :: ObfuscationConfig -> Transform -> Int -> Either AlgebircError ()
validateTransform cfg t degIn =
  let degOut = maxDegreeAfterTransform t degIn
      budget = cfgMaxDegree cfg
  in if degOut > budget
     then Left (DegreeOverflow degOut budget)
     else Right ()

-- | Tag name for reporting.
tagName :: TransformTag -> String
tagName AffineTransform      = "Affine"
tagName PolynomialTransform  = "PolySub"
tagName PermutationTransform = "Perm"
tagName CompositeTransform   = "Composite"
tagName SBoxTransform        = "S-Box"
tagName FeistelTransform     = "Feistel"
tagName PowerMapTransform    = "PowerMap"
tagName ARXDiffusionTransform = "ARX"
