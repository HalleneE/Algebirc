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
  ( -- * Homomorphic Pure Isogeny Pipeline
    homomorphicEncode
  , homomorphicDecode
    -- * Full Pipeline
  , geometricEncode
  , geometricDecode
    -- * Pipeline Configuration
  , GeometryConfig(..)
  , defaultGeometryConfig
  , deriveDynamicGeometry
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
import Algebirc.Geometry.SiegelModular
import Algebirc.Obfuscation.NonlinearTransform (mdsDiffusePure, mdsUndiffusePure)
import Data.List (foldl')
import Crypto.Hash (hashWith, SHA256(..))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

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
  , gcSiegelSteps  :: !Int
  , gcKey          :: SecretKey  -- ^ Key for deterministic derivation
  , gcVolcanoPath  :: [(VolcanoStep, Integer, Int)]
  } deriving (Show, Eq)


-- | Default configuration using well-chosen parameters.
defaultGeometryConfig :: Integer -> GeometryConfig
defaultGeometryConfig seed = 
  let p = 257  -- Fallback if not injected properly, but we expect caller to use config prime.
      -- We will dynamically generate this in the pipeline based on ObfuscationConfig
      key = SecretKey seed 3
      baseConf = GeometryConfig
        { gcPrime        = p
        , gcEcA          = 1
        , gcEcB          = (p - 1) `mod` p
        , gcIsogenySteps = [(7, 3), (13, 5), (31, 3)]
        , gcVolcanoPath  = generateVolcanoPath seed 2
        , gcCMExponents  = [(3, 2), (5, 1), (7, -1)]
        , gcCMSeed       = seed
        , gcHyperCoeffs  = [1, 0, p - 3, 0, 2, 1]
        , gcRichelotDepth = 2
        , gcSiegelEll    = 3
        , gcSiegelSteps  = 3
        , gcKey          = key
        }
  in baseConf

-- | Generate dynamic geometry config securely tied to the user's password seed
deriveDynamicGeometry :: ObfuscationConfig -> GeometryConfig
deriveDynamicGeometry cfg =
  let p = cfgFieldPrime cfg
      seed = cfgSeed cfg
      
      -- Helper: secure HKDF-like generation from seed + counter
      deriveKDF :: Int -> Integer
      deriveKDF counter =
        let seedStr = BSC.pack (show seed ++ ":" ++ show counter)
            digest = hashWith SHA256 seedStr
            bytes = BS.unpack (BA.convert digest)
        in foldl' (\acc b -> acc * 256 + fromIntegral b) 0 bytes
      
      -- Pseudo-random derivation based on seed
      s1 = deriveKDF 1 `mod` p
      s2 = deriveKDF 2 `mod` p
      s3 = deriveKDF 3 `mod` p
      
      -- Deterministic but password-dependent walk
      dynamicSteps = [ (s1, 3), (s2, 5), (s3, 3) ]
      
      -- Deterministic CM exponents
      cm1 = deriveKDF 4 `mod` 5 + 1
      cm2 = deriveKDF 5 `mod` 5 + 1
      cm3 = deriveKDF 6 `mod` 5 + 1
      dynamicCM = [ (3, fromIntegral cm1), (5, fromIntegral cm2), (7, fromIntegral cm3) ]
      
      -- Hyperelliptic coefficients derived from seed (ensuring non-zero high degree)
      hc5 = deriveKDF 7 `mod` (p - 1) + 1
      hc4 = deriveKDF 8 `mod` p
      hc3 = deriveKDF 9 `mod` p
      hc2 = deriveKDF 10 `mod` p
      hc1 = deriveKDF 11 `mod` p
      hc0 = deriveKDF 12 `mod` p
      dynamicHC = [hc0, hc1, hc2, hc3, hc4, hc5, 1]
      
      key = SecretKey seed 3
      
      -- VOLCANO PATH: Ascend -> Cycle -> Descend
      volcano = generateVolcanoPath seed 2
      
  in GeometryConfig
       { gcPrime        = p
       , gcEcA          = deriveKDF 13 `mod` p
       , gcEcB          = deriveKDF 14 `mod` p
       , gcIsogenySteps = dynamicSteps
       , gcVolcanoPath  = volcano
       , gcCMExponents  = dynamicCM
       , gcCMSeed       = seed
       , gcHyperCoeffs  = dynamicHC
       , gcRichelotDepth = 3  -- Increased depth
       , gcSiegelEll    = 3
       , gcSiegelSteps  = 3
       , gcKey          = key
       }

-- ============================================================
-- Homomorphic Pipeline: Pure Isogeny (Additive)
-- ============================================================

-- | Pure isogeny encoding (Linear/Homomorphic).
-- Skips non-linear ARX layers to preserve addition.
homomorphicEncode :: ObfuscationConfig -> [Integer] -> [Integer]
homomorphicEncode cfg coeffs =
  let geomCfg = deriveDynamicGeometry cfg
      -- Stage 1: Isogeny Walk
      step1 = stageIsogeny geomCfg coeffs
      -- Stage 2: CM Group Action
      step2 = stageCMAction geomCfg step1
      -- Stage 3: Richelot Isogeny
      step3 = stageRichelot geomCfg step2
      -- Stage 4: Siegel Modular Mix
      step4 = stageSiegel geomCfg step3
  in step4

-- | Pure isogeny decoding.
homomorphicDecode :: ObfuscationConfig -> [Integer] -> [Integer]
homomorphicDecode cfg coeffs =
  let geomCfg = deriveDynamicGeometry cfg
      -- Reverse stages without ARX
      step4 = stageSiegelInv geomCfg coeffs
      step3 = stageRichelotInv geomCfg step4
      step2 = stageCMActionInv geomCfg step3
      step1 = stageIsogenyInv geomCfg step2
  in step1

-- ============================================================
-- Full Pipeline: Encode (Obfuscate)
-- ============================================================

-- | Full geometric encoding pipeline.
geometricEncode :: ObfuscationConfig -> [Integer] -> [Integer]
geometricEncode cfg coeffs =
  let geomCfg = deriveDynamicGeometry cfg
      k  = gcKey geomCfg
      p  = gcPrime geomCfg
      -- Stage 1: Isogeny Walk
      step1 = stageIsogeny geomCfg coeffs
      
      -- Isogeny-SPN Interleaving: Pure MDS Diffusion 1
      step1_arx = mdsDiffusePure p (length step1) step1
      
      -- Stage 2: CM Group Action
      step2 = stageCMAction geomCfg step1_arx
      
      -- Isogeny-SPN Interleaving: Pure MDS Diffusion 2
      step2_arx = mdsDiffusePure p (length step2) step2
      
      -- Stage 3: Richelot Isogeny
      step3 = stageRichelot geomCfg step2_arx
      
      -- Isogeny-SPN Interleaving: Pure MDS Diffusion 3
      step3_arx = mdsDiffusePure p (length step3) step3
      
      -- Stage 4: Siegel Modular Mix
      step4 = stageSiegel geomCfg step3_arx
      
  in step4

-- | Full geometric decoding pipeline (inverse operations in reverse order).
geometricDecode :: ObfuscationConfig -> [Integer] -> [Integer]
geometricDecode cfg coeffs =
  let geomCfg = deriveDynamicGeometry cfg
      k  = gcKey geomCfg
      p  = gcPrime geomCfg

      -- Stage 4 inverse: Siegel unmix
      step4 = stageSiegelInv geomCfg coeffs
      
      -- Isogeny-SPN Interleaving: Inverse MDS Diffusion 3
      step3_arx = mdsUndiffusePure p (length step4) step4
      
      -- Stage 3 inverse: Richelot inverse
      step3 = stageRichelotInv geomCfg step3_arx
      
      -- Isogeny-SPN Interleaving: Inverse MDS Diffusion 2
      step2_arx = mdsUndiffusePure p (length step3) step3
      
      -- Stage 2 inverse: CM inverse permutation
      step2 = stageCMActionInv geomCfg step2_arx
      
      -- Isogeny-SPN Interleaving: Inverse MDS Diffusion 1
      step1_arx = mdsUndiffusePure p (length step2) step2
      
      -- Stage 1 inverse: Isogeny inverse transport
      step1 = stageIsogenyInv geomCfg step1_arx
      
  in step1

-- ============================================================
-- Individual Stages
-- ============================================================

-- | Stage 1: Transport coefficients through a volcano-structured isogeny chain.
stageIsogeny :: GeometryConfig -> [Integer] -> [Integer]
stageIsogeny cfg coeffs =
  let ec = EllipticCurve (gcEcA cfg) (gcEcB cfg) (gcPrime cfg)
  in volcanoMix ec (gcVolcanoPath cfg) coeffs

stageIsogenyInv :: GeometryConfig -> [Integer] -> [Integer]
stageIsogenyInv cfg coeffs =
  let ec = EllipticCurve (gcEcA cfg) (gcEcB cfg) (gcPrime cfg)
  in volcanoUnmix ec (gcVolcanoPath cfg) coeffs

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
-- Deprecated for Genus-1 linear payloads context to prevent Igusa dilution.
-- Holy Grail architecture natively relies on evaluator graphs instead.
stageRichelot :: GeometryConfig -> [Integer] -> [Integer]
stageRichelot cfg coeffs = coeffs

stageRichelotInv :: GeometryConfig -> [Integer] -> [Integer]
stageRichelotInv cfg coeffs = coeffs

-- | Stage 4: Final mixing via Siegel modular walk constants.
stageSiegel :: GeometryConfig -> [Integer] -> [Integer]
stageSiegel cfg coeffs =
  let p = gcPrime cfg
      hc = HyperCurve (iToV $ gcHyperCoeffs cfg) 2 p
      j0 = igusaInvariants hc
  in siegelMix p j0 (gcSiegelEll cfg) (gcSiegelSteps cfg) coeffs

stageSiegelInv :: GeometryConfig -> [Integer] -> [Integer]
stageSiegelInv cfg coeffs =
  let p = gcPrime cfg
      hc = HyperCurve (iToV $ gcHyperCoeffs cfg) 2 p
      j0 = igusaInvariants hc
  in siegelUnmix p j0 (gcSiegelEll cfg) (gcSiegelSteps cfg) coeffs
