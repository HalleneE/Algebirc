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
  , invertPipelinePolyWithScale
  , plAlgTransforms
  ) where

import Algebirc.Core.Types
import Algebirc.Obfuscation.Transform   (applyPipeline, invertPipeline)
import Algebirc.Obfuscation.NonlinearTransform (generatePipeline, applyNonlinear, invertNonlinear)
import Control.Monad (foldM)

-- ============================================================
-- Pipeline Record
-- ============================================================

data ObfuscationPipeline = ObfuscationPipeline
  { plAlgTransforms :: [Transform]
  }

-- ============================================================
-- Construction
-- ============================================================

buildPipeline :: ObfuscationConfig -> Either AlgebircError ObfuscationPipeline
buildPipeline cfg = do
  let seed = cfgSeed cfg
      sk   = SecretKey
               { skSeed     = seed
               , skPowerExp = 3
               }
  algTs <- generatePipeline cfg sk
  return ObfuscationPipeline
    { plAlgTransforms = algTs
    }

-- ============================================================
-- Forward: algebraic transforms -> geometric layer (Position-Safe)
-- ============================================================

runPipelinePoly :: ObfuscationConfig -> ObfuscationPipeline -> BoundedPoly -> Either AlgebircError BoundedPoly
runPipelinePoly cfg pl poly = do
  -- Step 1: algebraic transforms FIRST (including permutations)
  -- The state never drops back to structural integer arrays (Coefficient Bounce eliminated).
  applyPipelineUnified cfg (plAlgTransforms pl) poly

-- ============================================================
-- Inverse: geometric decode -> algebraic inverse (Position-Safe)
-- ============================================================

invertPipelinePoly :: ObfuscationConfig -> ObfuscationPipeline -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPipelinePoly cfg pl poly = invertPipelinePolyWithScale cfg pl poly 1

invertPipelinePolyWithScale :: ObfuscationConfig -> ObfuscationPipeline -> BoundedPoly -> Int -> Either AlgebircError BoundedPoly
invertPipelinePolyWithScale cfg pl poly scalePower = do
  -- Step 1: invert algebraic transforms (pure domain, no integer extraction needed)
  invertPipelineUnifiedWithScale cfg (plAlgTransforms pl) poly scalePower

-- ============================================================
-- Helpers
-- ============================================================

applyPipelineUnified :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
applyPipelineUnified cfg ts p = foldM (\acc t -> applyUnified cfg t acc) p ts

applyUnified :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applyUnified cfg t poly = case transformTag t of
  PowerMapTransform     -> applyNonlinear cfg t poly
  ARXDiffusionTransform -> applyNonlinear cfg t poly
  _                     -> applyPipeline cfg [t] poly

invertPipelineUnified :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
invertPipelineUnified cfg ts p = invertPipelineUnifiedWithScale cfg ts p 1

invertPipelineUnifiedWithScale :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Int -> Either AlgebircError BoundedPoly
invertPipelineUnifiedWithScale cfg ts p scalePower = 
  foldM (\acc t -> invertUnifiedWithScale cfg t acc scalePower) p (reverse ts)

invertUnified :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
invertUnified cfg t poly = invertUnifiedWithScale cfg t poly 1

invertUnifiedWithScale :: ObfuscationConfig -> Transform -> BoundedPoly -> Int -> Either AlgebircError BoundedPoly
invertUnifiedWithScale cfg t poly scalePower = case transformTag t of
  PowerMapTransform     -> invertNonlinear cfg t poly
  ARXDiffusionTransform -> invertNonlinear cfg t poly
  AffineTransform       -> invertAffineWithScale cfg t poly scalePower
  _                     -> invertPipeline cfg [t] poly

-- | Handle Affine Inversion with higher scale power (a^n).
-- Dec(Y) = (Y - (scalePower * b)) / (a^scalePower)
invertAffineWithScale :: ObfuscationConfig -> Transform -> BoundedPoly -> Int -> Either AlgebircError BoundedPoly
invertAffineWithScale cfg t poly scalePower = do
  let p = cfgFieldPrime cfg
      maxDeg = polyMaxDegree poly
      a = case transformA t of { Just v -> v; _ -> 1 }
      b = case transformB t of { Just v -> v; _ -> 0 }
      
      -- Scale "a" and "b" to the given power
      -- Note: In addition, b becomes (scalePower * b)
      -- In multiplication, it becomes a mess, but for Pure Isogeny with b=0,
      -- we only care about a^scalePower.
      aPow = powMod a (fromIntegral scalePower) p
      aInv = modInv aPow p
      
      -- If b=0 (Homomorphic mode), this is just Y / a^n
      -- If b/=0, this only works for addition.
      bScaled = (fromIntegral scalePower * b) `mod` p
      
      newTerms = [ Term (((getCoeffAt i poly - bScaled + p) * aInv) `mod` p) i 
                 | i <- [0..maxDeg] ]
  Right $ mkBoundedPoly p maxDeg newTerms

-- Modular inverse helper
modInv :: Integer -> Integer -> Integer
modInv n p = let (g, x, _) = extGcd n p in if g == 1 then (x `mod` p + p) `mod` p else 0

extGcd :: Integer -> Integer -> (Integer, Integer, Integer)
extGcd 0 b = (b, 0, 1)
extGcd a b = let (g, x, y) = extGcd (b `mod` a) a
             in (g, y - (b `div` a) * x, x)

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod b e m = if even e 
               then powMod ((b*b) `mod` m) (e `div` 2) m
               else (b * powMod b (e-1) m) `mod` m
