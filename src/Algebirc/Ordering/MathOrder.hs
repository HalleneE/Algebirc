-- |
-- Module      : Algebirc.Ordering.MathOrder
-- Description : Mathematical priority computation from hash fingerprints
-- License     : MIT
--
-- Uses the SHA-256 hash to compute a mathematical priority for each file.
-- The priority determines execution order in the obfuscation pipeline.
--
-- = Priority Formula
--
-- Given hash H = h_0 h_1 ... h_31 (32 bytes), the priority is computed as:
--
-- @
-- priority(H) = ∑_{i=0}^{31} h_i * g^i  (mod p)
-- @
--
-- where g is a generator of GF(p)* and p is the field prime.
-- This maps hashes into the finite field and gives a total ordering.

module Algebirc.Ordering.MathOrder
  ( -- * Priority Computation
    computePriority
  , computeRelativeOrder
    -- * Ordering
  , orderByPriority
  , OrderedItem(..)
  ) where

import Algebirc.Core.Types
import Algebirc.Core.FiniteField
import Algebirc.Ordering.Fingerprint
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.Ord (comparing)

-- ============================================================
-- Types
-- ============================================================

-- | An item with its computed execution priority.
data OrderedItem a = OrderedItem
  { oiItem     :: !a
  , oiPriority :: !Integer
  , oiIndex    :: !Int        -- ^ Original index
  , oiOrder    :: !Int        -- ^ Execution order (0-based)
  } deriving (Show, Eq)

-- ============================================================
-- Priority Computation
-- ============================================================

-- | Compute mathematical priority from a fingerprint.
--
-- Formula: priority = ∑ h_i * g^i (mod p)
-- where g = 3 (primitive root for common primes) and p is field prime.
computePriority :: ObfuscationConfig -> FileFingerprint -> Integer
computePriority cfg fp =
  let p = cfgFieldPrime cfg
      g = 3  -- generator (primitive root)
      hashBytes = hashBytesToList (fpHash fp)
      -- Compute ∑ h_i * g^i (mod p)
      terms = zipWith (\h i -> (h * powMod g i p) `mod` p) hashBytes [0 :: Integer ..]
      total = foldl (\acc x -> (acc + x) `mod` p) 0 terms
  in total

-- | Compute relative priority between two fingerprints.
-- Returns negative if fp1 should execute before fp2.
computeRelativeOrder :: ObfuscationConfig -> FileFingerprint -> FileFingerprint -> Ordering
computeRelativeOrder cfg fp1 fp2 =
  compare (computePriority cfg fp1) (computePriority cfg fp2)

-- ============================================================
-- Ordering
-- ============================================================

-- | Order a list of items by their fingerprint priority.
-- Returns items in execution order (lowest priority first).
orderByPriority :: ObfuscationConfig -> [(a, FileFingerprint)] -> [OrderedItem a]
orderByPriority cfg items =
  let -- Compute priorities
      withPriority = zipWith (\idx (item, fp) ->
        OrderedItem
          { oiItem     = item
          , oiPriority = computePriority cfg fp
          , oiIndex    = idx
          , oiOrder    = 0  -- filled after sort
          }) [0..] items
      -- Sort by priority
      sorted = sortBy (comparing oiPriority) withPriority
      -- Assign execution order
      ordered = zipWith (\order oi -> oi { oiOrder = order }) [0..] sorted
  in ordered

-- ============================================================
-- Helpers
-- ============================================================

hashBytesToList :: BS.ByteString -> [Integer]
hashBytesToList = map fromIntegral . BS.unpack

-- | Modular exponentiation.
powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 m = 1 `mod` m
powMod b e m
  | even e    = let h = powMod b (e `div` 2) m in (h * h) `mod` m
  | otherwise = (b * powMod b (e - 1) m) `mod` m
