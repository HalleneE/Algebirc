-- |
-- Module      : Algebirc.Geometry.GeometricPipeline
-- Description : Orchestrator for the algebraic geometry obfuscation layer
-- License     : MIT
--
-- = Pipeline
--
-- Polynomial coefficients → EC lift → Isogeny walk → CM action
-- → Genus-2 Jacobian → Richelot isogeny → Siegel mix → Project back
--
-- Every layer adds geometric complexity that an attacker must
-- reverse through algebraic geometry (not just polynomial algebra).

module Algebirc.Geometry.GeometricPipeline
  ( -- * Full Pipeline
    geometricEncode
  , geometricDecode
    -- * Pipeline Configuration
  , GeometryConfig(..)
  , defaultGeometryConfig
    -- * Individual Stages (for analysis)
  , stageIsogeny
  , stageCMAction
  , stageRichelot
  , stageSiegel
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve
import Algebirc.Geometry.Isogeny
import Algebirc.Geometry.CMAction
import Algebirc.Geometry.HyperellipticCurve
import Algebirc.Geometry.RichelotIsogeny
import Algebirc.Geometry.SiegelModular
import Data.List (foldl')

-- ============================================================
-- Pipeline Configuration
-- ============================================================

-- | Configuration for the geometric obfuscation pipeline.
data GeometryConfig = GeometryConfig
  { gcPrime        :: !Integer   -- ^ Field prime p
  , gcEcA          :: !Integer   -- ^ EC parameter a (y²=x³+ax+b)
  , gcEcB          :: !Integer   -- ^ EC parameter b
  , gcIsogenySteps :: ![(Integer, Int)]  -- ^ Isogeny walk: [(seed, ℓ)]
  , gcCMExponents  :: ![(Integer, Int)]  -- ^ CM action: [(ℓ, exponent)]
  , gcCMSeed       :: !Integer   -- ^ CM permutation seed
  , gcHyperCoeffs  :: ![Integer] -- ^ Genus-2 sextic f(x) coefficients
  , gcRichelotDepth :: !Int      -- ^ Number of Richelot steps
  , gcSiegelEll    :: !Int       -- ^ Siegel modular polynomial degree ℓ
  , gcSiegelSteps  :: !Int       -- ^ Siegel walk length
  } deriving (Show, Eq)

-- | Default configuration using well-chosen parameters.
defaultGeometryConfig :: Integer -> GeometryConfig
defaultGeometryConfig p = GeometryConfig
  { gcPrime        = p
  , gcEcA          = 1
  , gcEcB          = (p - 1) `mod` p  -- a = 1, b = -1 → y² = x³ + x - 1
  , gcIsogenySteps = [(7, 3), (13, 5), (31, 3)]  -- 3-step walk
  , gcCMExponents  = [(3, 2), (5, 1), (7, -1)]   -- CM action path
  , gcCMSeed       = 42
  , gcHyperCoeffs  = [1, 0, p - 3, 0, 2, 1]  -- y² = x⁵ + 2x⁴ - 3x² + 1
  , gcRichelotDepth = 2
  , gcSiegelEll    = 3
  , gcSiegelSteps  = 3
  }

-- ============================================================
-- Full Pipeline: Encode (Obfuscate)
-- ============================================================

-- | Full geometric encoding pipeline.
-- Takes polynomial coefficients and wraps them in geometric structure.
--
-- Pipeline:
-- 1. EC isogeny walk (coefficient transport)
-- 2. CM group action (j-invariant permutation)
-- 3. Richelot isogeny (genus-2 mixing via Igusa)
-- 4. Siegel modular walk (final mixing constants)
geometricEncode :: GeometryConfig -> [Integer] -> [Integer]
geometricEncode cfg coeffs =
  let p = gcPrime cfg
      
      -- Stage 1: Isogeny Walk
      step1 = stageIsogeny cfg coeffs
      
      -- Stage 2: CM Group Action
      step2 = stageCMAction cfg step1
      
      -- Stage 3: Richelot Isogeny
      step3 = stageRichelot cfg step2
      
      -- Stage 4: Siegel Modular Mix
      step4 = stageSiegel cfg step3
      
  in step4

-- | Full geometric decoding pipeline (inverse operations in reverse order).
geometricDecode :: GeometryConfig -> [Integer] -> [Integer]
geometricDecode cfg coeffs =
  let -- Stage 4 inverse: Siegel unmix
      step4 = stageSiegelInv cfg coeffs
      
      -- Stage 3 inverse: Richelot inverse
      step3 = stageRichelotInv cfg step4
      
      -- Stage 2 inverse: CM inverse permutation
      step2 = stageCMActionInv cfg step3
      
      -- Stage 1 inverse: Isogeny inverse transport
      step1 = stageIsogenyInv cfg step2
      
  in step1

-- ============================================================
-- Individual Stages
-- ============================================================

-- | Stage 1: Transport coefficients through an isogeny chain.
stageIsogeny :: GeometryConfig -> [Integer] -> [Integer]
stageIsogeny cfg coeffs =
  let ec = EllipticCurve (gcEcA cfg) (gcEcB cfg) (gcPrime cfg)
  in transportCoeffs ec (gcIsogenySteps cfg) coeffs

stageIsogenyInv :: GeometryConfig -> [Integer] -> [Integer]
stageIsogenyInv cfg coeffs =
  let ec = EllipticCurve (gcEcA cfg) (gcEcB cfg) (gcPrime cfg)
  in inverseTransportCoeffs ec (gcIsogenySteps cfg) coeffs

-- | Stage 2: Permute via CM group action on j-invariants.
stageCMAction :: GeometryConfig -> [Integer] -> [Integer]
stageCMAction cfg coeffs =
  let ec = EllipticCurve (gcEcA cfg) (gcEcB cfg) (gcPrime cfg)
  in cmPermute ec (gcCMSeed cfg) coeffs

stageCMActionInv :: GeometryConfig -> [Integer] -> [Integer]
stageCMActionInv cfg coeffs =
  let ec = EllipticCurve (gcEcA cfg) (gcEcB cfg) (gcPrime cfg)
  in cmInversePermute ec (gcCMSeed cfg) coeffs

-- | Stage 3: Mix via Richelot (2,2)-isogeny on genus-2 Jacobian.
stageRichelot :: GeometryConfig -> [Integer] -> [Integer]
stageRichelot cfg coeffs =
  let hc = HyperCurve (gcHyperCoeffs cfg) 2 (gcPrime cfg)
  in richelotTransport hc (gcRichelotDepth cfg) coeffs

stageRichelotInv :: GeometryConfig -> [Integer] -> [Integer]
stageRichelotInv cfg coeffs =
  let hc = HyperCurve (gcHyperCoeffs cfg) 2 (gcPrime cfg)
  in richelotInverseTransport hc (gcRichelotDepth cfg) coeffs

-- | Stage 4: Final mixing via Siegel modular walk constants.
stageSiegel :: GeometryConfig -> [Integer] -> [Integer]
stageSiegel cfg coeffs =
  let p = gcPrime cfg
      hc = HyperCurve (gcHyperCoeffs cfg) 2 p
      j0 = igusaInvariants hc
  in siegelMix p j0 (gcSiegelEll cfg) (gcSiegelSteps cfg) coeffs

stageSiegelInv :: GeometryConfig -> [Integer] -> [Integer]
stageSiegelInv cfg coeffs =
  let p = gcPrime cfg
      hc = HyperCurve (gcHyperCoeffs cfg) 2 p
      j0 = igusaInvariants hc
  in siegelUnmix p j0 (gcSiegelEll cfg) (gcSiegelSteps cfg) coeffs
