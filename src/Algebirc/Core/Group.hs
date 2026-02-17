-- |
-- Module      : Algebirc.Core.Group
-- Description : Permutation group operations for byte-level shuffling
-- License     : MIT
--
-- = Formal Properties
--
-- * __Closure__: compose of two permutations on {0..n-1} is a permutation
-- * __Associativity__: (σ ∘ τ) ∘ ρ = σ ∘ (τ ∘ ρ)
-- * __Identity__: id(i) = i
-- * __Inverse__: σ ∘ σ⁻¹ = id

module Algebirc.Core.Group
  ( -- * Construction
    identityPerm
  , transposition
  , cyclicPerm
  , fromCycles
    -- * Operations
  , applyPerm
  , composePerm
  , invertPerm
  , permOrder
    -- * Representations
  , toCycles
  , toOneLine
    -- * Data Operations
  , permuteBytes
  , unpermuteBytes
    -- * Generation
  , generateFromSeed
  ) where

import Algebirc.Core.Types
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Data.List (unfoldr)

-- ============================================================
-- Construction
-- ============================================================

-- | Identity permutation on {0..n-1}.
identityPerm :: Int -> Permutation
identityPerm n = Permutation n (Map.fromList [(i, i) | i <- [0..n-1]])

-- | Transposition: swap elements at positions i and j.
transposition :: Int -> Int -> Int -> Either AlgebircError Permutation
transposition n i j
  | i < 0 || i >= n || j < 0 || j >= n = Left (PermutationError "Index out of range")
  | otherwise =
      let mapping = Map.fromList $
            [ (k, if k == i then j else if k == j then i else k) | k <- [0..n-1] ]
      in Right (Permutation n mapping)

-- | Cyclic permutation: (0 1 2 ... n-1) -> (1 2 3 ... n-1 0).
cyclicPerm :: Int -> Permutation
cyclicPerm n = Permutation n (Map.fromList [(i, (i + 1) `mod` n) | i <- [0..n-1]])

-- | Construct permutation from cycle notation.
-- E.g., @fromCycles 5 [[0,2,4],[1,3]]@ = (0→2, 2→4, 4→0, 1→3, 3→1).
fromCycles :: Int -> [[Int]] -> Either AlgebircError Permutation
fromCycles n cycles =
  let -- Start with identity
      idMap = Map.fromList [(i, i) | i <- [0..n-1]]
      -- Apply each cycle
      applyCycle m [] = m
      applyCycle m [_] = m  -- single element cycle = identity
      applyCycle m cyc =
        let pairs = zip cyc (tail cyc ++ [head cyc])
        in foldl (\acc (from, to) -> Map.insert from to acc) m pairs
      result = foldl applyCycle idMap cycles
  in if Map.size result /= n
     then Left (PermutationError "Invalid cycle notation")
     else Right (Permutation n result)

-- ============================================================
-- Operations
-- ============================================================

-- | Apply permutation to an index.
applyPerm :: Permutation -> Int -> Int
applyPerm (Permutation _ m) i = Map.findWithDefault i i m

-- | Compose two permutations: (σ ∘ τ)(i) = σ(τ(i)).
composePerm :: Permutation -> Permutation -> Either AlgebircError Permutation
composePerm s t
  | permSize s /= permSize t = Left (PermutationError "Size mismatch")
  | otherwise =
      let n = permSize s
          composed = Map.fromList
            [ (i, applyPerm s (applyPerm t i)) | i <- [0..n-1] ]
      in Right (Permutation n composed)

-- | Inverse permutation: σ⁻¹ such that σ ∘ σ⁻¹ = id.
invertPerm :: Permutation -> Permutation
invertPerm (Permutation n m) =
  Permutation n (Map.fromList [(v, k) | (k, v) <- Map.toList m])

-- | Order of a permutation: smallest k > 0 such that σ^k = id.
-- Equals LCM of cycle lengths.
permOrder :: Permutation -> Int
permOrder perm =
  let cycles = toCycles perm
      lengths = map length cycles
  in if null lengths then 1 else foldl1 lcm lengths

-- ============================================================
-- Cycle Representation
-- ============================================================

-- | Convert permutation to cycle notation (non-trivial cycles only).
toCycles :: Permutation -> [[Int]]
toCycles (Permutation n m) = go [0..n-1] []
  where
    go [] acc = reverse acc
    go (i:rest) acc =
      let cyc = traceCycle i
      in if length cyc <= 1
         then go rest acc
         else go (filter (`notElem` cyc) rest) (cyc : acc)
    traceCycle start = start : unfoldr step (applyPerm (Permutation n m) start)
      where
        step current
          | current == start = Nothing
          | otherwise        = Just (current, applyPerm (Permutation n m) current)

-- | One-line notation: [σ(0), σ(1), ..., σ(n-1)].
toOneLine :: Permutation -> [Int]
toOneLine (Permutation n m) = [Map.findWithDefault i i m | i <- [0..n-1]]

-- ============================================================
-- Data Operations
-- ============================================================

-- | Permute bytes of a ByteString.
-- If ByteString length /= permutation size, pads or truncates.
permuteBytes :: Permutation -> BS.ByteString -> BS.ByteString
permuteBytes perm bs =
  let n = permSize perm
      padded = BS.take n (bs `BS.append` BS.replicate (max 0 (n - BS.length bs)) 0)
      permuted = BS.pack [ BS.index padded (applyPerm perm i) | i <- [0..n-1] ]
  in permuted

-- | Inverse-permute bytes (undo permutation).
unpermuteBytes :: Permutation -> BS.ByteString -> BS.ByteString
unpermuteBytes perm = permuteBytes (invertPerm perm)

-- ============================================================
-- Deterministic Generation from Seed
-- ============================================================

-- | Generate a permutation of size n from an integer seed.
-- Uses Fisher-Yates-like deterministic shuffle.
generateFromSeed :: Int -> Integer -> Permutation
generateFromSeed n seed =
  let -- Simple deterministic PRNG: linear congruential
      prng s = (s * 6364136223846793005 + 1442695040888963407) `mod` (2^(62 :: Integer))
      -- Fisher-Yates shuffle
      go [] _ acc = acc
      go [x] _ acc = x : acc
      go xs s acc =
        let s' = prng s
            idx = fromIntegral (s' `mod` fromIntegral (length xs))
        in case splitAt idx xs of
             (before, chosen:after) -> go (before ++ after) s' (chosen : acc)
             _ -> acc  -- safety: should not happen
      shuffled = go [0..n-1] seed []
      mapping = zip [0..n-1] shuffled
  in Permutation n (Map.fromList mapping)

-- ============================================================
-- AlgebraicStructure Instance
-- ============================================================

instance AlgebraicStructure Permutation where
  identity = identityPerm 256  -- default byte-level
  compose  = composePerm
  inverse  = Right . invertPerm
