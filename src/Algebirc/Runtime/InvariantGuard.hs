-- |
-- Module      : Algebirc.Runtime.InvariantGuard
-- Description : Geometric invariant-based tamper detection
-- License     : MIT
--
-- = Security Model
--
-- Uses j-invariant (EC) and Igusa invariants (genus-2) as tamper checksums.
-- Expected values are DERIVED from transform parameters (not stored as literals).
-- On tamper detection: produce WRONG output silently (no crash = no oracle).
--
-- The key insight: geometric invariants are preserved under valid operations.
-- If coefficients are modified mid-pipeline, invariants break.

{-# LANGUAGE BangPatterns #-}

module Algebirc.Runtime.InvariantGuard
  ( -- * Types
    InvariantCheckpoint(..)
  , GuardResult(..)
    -- * Checkpoint Derivation (compile-time)
  , deriveECCheckpoint
  , deriveIgusaCheckpoint
  , deriveDegreeCheckpoint
  , deriveFullCheckpoint
    -- * Runtime Validation
  , validateEC
  , validateIgusa
  , validateDegree
  , validateFull
    -- * Silent Corruption
  , corruptOnFailure
  , guardedApply
  ) where

import Data.Word (Word64)
import Data.Bits (xor, shiftR)
import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve (jInvariant, modPow)
import Algebirc.Geometry.HyperellipticCurve (igusaInvariants)

-- ============================================================
-- Types
-- ============================================================

-- | A tamper-detection checkpoint derived from transform parameters.
-- Expected values are computed at compile-time, NOT stored as literals.
data InvariantCheckpoint = InvariantCheckpoint
  { icJInvariant :: !(Maybe Integer)        -- ^ Expected j-invariant (EC transforms)
  , icIgusa      :: !(Maybe IgusaInvariants) -- ^ Expected Igusa (genus-2 transforms)
  , icDegree     :: !Int                     -- ^ Expected polynomial degree
  , icChecksum   :: !Word64                  -- ^ Derived checksum of all fields
  } deriving (Show, Eq)

-- | Result of invariant validation.
data GuardResult
  = GuardPass          -- ^ All invariants match
  | GuardTampered      -- ^ Tamper detected — will silently corrupt
  deriving (Show, Eq)

-- ============================================================
-- Checkpoint Derivation (compile-time)
-- ============================================================

-- | Derive j-invariant checkpoint from an elliptic curve.
deriveECCheckpoint :: EllipticCurve -> InvariantCheckpoint
deriveECCheckpoint ec =
  let !j = jInvariant ec
  in InvariantCheckpoint
    { icJInvariant = Just j
    , icIgusa      = Nothing
    , icDegree     = 0
    , icChecksum   = checksumFromJ j (ecPrime ec)
    }

-- | Derive Igusa checkpoint from a hyperelliptic curve.
deriveIgusaCheckpoint :: HyperCurve -> InvariantCheckpoint
deriveIgusaCheckpoint hc =
  let !igusa = igusaInvariants hc
  in InvariantCheckpoint
    { icJInvariant = Nothing
    , icIgusa      = Just igusa
    , icDegree     = 0
    , icChecksum   = checksumFromIgusa igusa (hcPrime hc)
    }

-- | Derive degree checkpoint.
deriveDegreeCheckpoint :: Int -> InvariantCheckpoint
deriveDegreeCheckpoint d = InvariantCheckpoint
  { icJInvariant = Nothing
  , icIgusa      = Nothing
  , icDegree     = d
  , icChecksum   = fromIntegral d * 0x9E3779B97F4A7C15  -- golden ratio hash
  }

-- | Derive full checkpoint from transform parameters.
-- The checksum is DERIVED from the transform, not hardcoded.
deriveFullCheckpoint :: Maybe EllipticCurve -> Maybe HyperCurve -> Int -> InvariantCheckpoint
deriveFullCheckpoint mec mhc deg =
  let mj = fmap jInvariant mec
      migusa = fmap igusaInvariants mhc
      p = case mec of
            Just ec -> ecPrime ec
            Nothing -> case mhc of
                         Just hc -> hcPrime hc
                         Nothing -> 257  -- default
      !cs = combineChecksums
              (maybe 0 (\j -> checksumFromJ j p) mj)
              (maybe 0 (\ig -> checksumFromIgusa ig p) migusa)
              (fromIntegral deg)
  in InvariantCheckpoint
    { icJInvariant = mj
    , icIgusa      = migusa
    , icDegree     = deg
    , icChecksum   = cs
    }

-- ============================================================
-- Checksum Computation (non-trivial to reverse)
-- ============================================================

-- | Checksum from j-invariant.  Uses mixing function.
checksumFromJ :: Integer -> Integer -> Word64
checksumFromJ j p =
  let !h1 = fromIntegral (j `mod` (toInteger (maxBound :: Word64)))
      !h2 = fromIntegral (p `mod` (toInteger (maxBound :: Word64)))
  in mixHash h1 h2

-- | Checksum from Igusa invariants.
checksumFromIgusa :: IgusaInvariants -> Integer -> Word64
checksumFromIgusa (IgusaInvariants j2 j4 j6 j10) p =
  let !h1 = fromIntegral (j2 `mod` toInteger (maxBound :: Word64))
      !h2 = fromIntegral (j4 `mod` toInteger (maxBound :: Word64))
      !h3 = fromIntegral (j6 `mod` toInteger (maxBound :: Word64))
      !h4 = fromIntegral (j10 `mod` toInteger (maxBound :: Word64))
      !hp = fromIntegral (p `mod` toInteger (maxBound :: Word64))
  in mixHash (mixHash (mixHash h1 h2) (mixHash h3 h4)) hp

-- | Combine multiple checksums.
combineChecksums :: Word64 -> Word64 -> Word64 -> Word64
combineChecksums a b c = mixHash (mixHash a b) c

-- | Non-cryptographic mixing function (MurmurHash3 finalizer).
mixHash :: Word64 -> Word64 -> Word64
mixHash h1 h2 =
  let !k  = h2 * 0xFF51AFD7ED558CCD
      !k' = k `xor` (k `shiftR` 33)
      !k'' = k' * 0xC4CEB9FE1A85EC53
      !k''' = k'' `xor` (k'' `shiftR` 33)
  in h1 `xor` k'''

-- ============================================================
-- Runtime Validation
-- ============================================================

-- | Validate j-invariant matches expected.
validateEC :: InvariantCheckpoint -> EllipticCurve -> GuardResult
validateEC cp ec =
  case icJInvariant cp of
    Nothing       -> GuardPass  -- no EC checkpoint
    Just expected ->
      let !actual = jInvariant ec
      in if actual == expected then GuardPass else GuardTampered

-- | Validate Igusa invariants match expected.
validateIgusa :: InvariantCheckpoint -> HyperCurve -> GuardResult
validateIgusa cp hc =
  case icIgusa cp of
    Nothing       -> GuardPass  -- no Igusa checkpoint
    Just expected ->
      let !actual = igusaInvariants hc
      in if actual == expected then GuardPass else GuardTampered

-- | Validate polynomial degree hasn't changed.
validateDegree :: InvariantCheckpoint -> [a] -> GuardResult
validateDegree cp coeffs =
  let !expectedDeg = icDegree cp
      !actualDeg   = length coeffs - 1
  in if expectedDeg == 0 || actualDeg == expectedDeg
     then GuardPass
     else GuardTampered

-- | Full validation: all applicable invariants.
validateFull :: InvariantCheckpoint -> Maybe EllipticCurve -> Maybe HyperCurve -> [a] -> GuardResult
validateFull cp mec mhc coeffs =
  let ecResult = case mec of
        Just ec -> validateEC cp ec
        Nothing -> GuardPass
      igResult = case mhc of
        Just hc -> validateIgusa cp hc
        Nothing -> GuardPass
      degResult = validateDegree cp coeffs
  in if all (== GuardPass) [ecResult, igResult, degResult]
     then GuardPass
     else GuardTampered

-- ============================================================
-- Silent Corruption (no crash = no oracle)
-- ============================================================

-- | If tampered, silently corrupt the output.
-- The corruption is deterministic but produces wrong results.
-- Attacker gets no "tamper detected" signal.
corruptOnFailure :: GuardResult -> Integer -> [Integer] -> [Integer]
corruptOnFailure GuardPass _ coeffs = coeffs
corruptOnFailure GuardTampered p coeffs =
  -- XOR each coefficient with a corruption pattern
  -- Pattern derived from coefficient position → deterministic but wrong
  [ (c + fromIntegral i * 0x5DEECE66D + 0xB) `mod` p
  | (i, c) <- zip [(0::Integer)..] coeffs ]

-- | Apply a function with invariant guard.
-- If checkpoint fails, silently corrupt output.
guardedApply :: InvariantCheckpoint
             -> Integer                          -- ^ Prime
             -> Maybe EllipticCurve              -- ^ EC context
             -> Maybe HyperCurve                 -- ^ Genus-2 context
             -> ([Integer] -> [Integer])          -- ^ Transform function
             -> [Integer]                        -- ^ Input coefficients
             -> [Integer]                        -- ^ Output (correct or corrupted)
guardedApply cp p mec mhc f coeffs =
  let !result = f coeffs
      !guard  = validateFull cp mec mhc result
  in corruptOnFailure guard p result
