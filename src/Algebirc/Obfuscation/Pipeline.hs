-- |
-- Module      : Algebirc.Obfuscation.Pipeline
-- Description : Genus-dispatched obfuscation pipeline orchestrator
-- License     : MIT

module Algebirc.Obfuscation.Pipeline
  ( -- * Pipeline Type
    ObfuscationPipeline(..)
    -- * Construction
  , buildPipeline
    -- * Forward / Inverse
  , runPipelinePoly
  , invertPipelinePoly
  , plAlgTransforms
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.GeometricPipeline
  ( geometricEncode, geometricDecode, defaultGeometryConfig )
import Algebirc.Obfuscation.Transform   (applyPipeline, invertPipeline)
import Algebirc.Obfuscation.NonlinearTransform (generatePipeline, applyNonlinear, invertNonlinear)
import Control.Monad (foldM)

-- ============================================================
-- Pipeline Record
-- ============================================================

data ObfuscationPipeline = ObfuscationPipeline
  { plGeomEncode  :: [Integer] -> [Integer]
  , plGeomDecode  :: [Integer] -> [Integer]
  , plAlgTransforms :: [Transform]
  }

-- ============================================================
-- Construction
-- ============================================================

buildPipeline :: ObfuscationConfig -> Either AlgebircError ObfuscationPipeline
buildPipeline cfg = do
  let seed = cfgSeed cfg
      sk   = SecretKey
               { skSeed     = seed
               , skRounds   = 4
               , skSBoxSeed = seed + 1
               , skPowerExp = 3
               }
  algTs <- generatePipeline cfg sk
  let (geomEnc, geomDec) = case cfgGenus cfg of
        2 -> let gcfg = defaultGeometryConfig (cfgFieldPrime cfg)
             in (geometricEncode gcfg, geometricDecode gcfg)
        _ -> (id, id)
  return ObfuscationPipeline
    { plGeomEncode    = geomEnc
    , plGeomDecode    = geomDec
    , plAlgTransforms = algTs
    }

-- ============================================================
-- Forward: algebraic transforms -> geometric layer (Position-Safe)
-- ============================================================

runPipelinePoly :: ObfuscationConfig -> ObfuscationPipeline -> BoundedPoly -> Either AlgebircError BoundedPoly
runPipelinePoly cfg pl poly = do
  -- Step 1: algebraic transforms FIRST (including permutations)
  algPoly <- applyPipelineUnified cfg (plAlgTransforms pl) poly
  
  -- Step 2: geometric encode on the final positions
  let p         = cfgFieldPrime cfg
      maxDeg    = polyMaxDegree algPoly
      coeffs    = [ getCoeffAt i algPoly | i <- [0 .. maxDeg] ]
      geoCoeffs = plGeomEncode pl coeffs
      geoTerms  = zipWith (\i c -> Term c i) [0..] geoCoeffs
  return $ mkBoundedPoly p maxDeg geoTerms

-- ============================================================
-- Inverse: geometric decode -> algebraic inverse (Position-Safe)
-- ============================================================

invertPipelinePoly :: ObfuscationConfig -> ObfuscationPipeline -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPipelinePoly cfg pl poly = do
  -- Step 1: geometric decode FIRST (strip invariants from current positions)
  let p          = cfgFieldPrime cfg
      maxDeg     = polyMaxDegree poly
      coeffs     = [ getCoeffAt i poly | i <- [0 .. maxDeg] ]
      ungeoCoeffs = plGeomDecode pl coeffs
      ungeoTerms  = zipWith (\i c -> Term c i) [0..] ungeoCoeffs
      ungeoPoly   = mkBoundedPoly p maxDeg ungeoTerms
      
  -- Step 2: invert algebraic transforms
  invertPipelineUnified cfg (plAlgTransforms pl) ungeoPoly

-- ============================================================
-- Helpers
-- ============================================================

applyPipelineUnified :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPipelineUnified cfg ts p = foldM (\acc t -> applyUnified cfg t acc) p ts

applyUnified :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyUnified cfg t poly = case transformTag t of
  SBoxTransform         -> applyNonlinear cfg t poly
  FeistelTransform      -> applyNonlinear cfg t poly
  PowerMapTransform     -> applyNonlinear cfg t poly
  ARXDiffusionTransform -> applyNonlinear cfg t poly
  _                     -> applyPipeline cfg [t] poly

invertPipelineUnified :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPipelineUnified cfg ts p = foldM (\acc t -> invertUnified cfg t acc) p (reverse ts)

invertUnified :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertUnified cfg t poly = case transformTag t of
  SBoxTransform         -> invertNonlinear cfg t poly
  FeistelTransform      -> invertNonlinear cfg t poly
  PowerMapTransform     -> invertNonlinear cfg t poly
  ARXDiffusionTransform -> invertNonlinear cfg t poly
  _                     -> invertPipeline cfg [t] poly
