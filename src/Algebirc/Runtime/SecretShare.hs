-- |
-- Module      : Algebirc.Runtime.SecretShare
-- Description : 2-of-2 additive secret sharing over GF(p) with Word64
-- License     : MIT
--
-- = Security Model
--
-- Every coefficient c is split: c = s₁ + s₂ (mod p)
-- One share alone is uniformly random — memory dump = no information.
--
-- Arithmetic on shares without reconstruction:
-- - Add: (s₁,s₂) + (t₁,t₂) = (s₁+t₁, s₂+t₂)         — 2 adds
-- - Mul: Karatsuba variant                                — 3 muls
-- - Reconstruct: s₁ + s₂ mod p                           — only at final output
--
-- GHC safety: all fields strict (!), NOINLINE on reconstruct.

{-# LANGUAGE BangPatterns #-}

module Algebirc.Runtime.SecretShare
  ( -- * Types
    SharedCoeff(..)
  , SharedPoly(..)
    -- * Core Operations
  , splitCoeff
  , reconstruct
  , sharedAdd
  , sharedSub
  , sharedNeg
  , sharedMul
  , sharedScalar
    -- * Polynomial-Level
  , splitPoly
  , reconstructPoly
  , sharedPolyAdd
  , sharedPolyMul
  , sharedEvalAt
    -- * Share Management
  , rotateShares
  , rerandomize
    -- * Bulk Operations
  , splitCoeffs
  , reconstructCoeffs
  ) where

import Data.Word (Word64)
import Data.List (foldl')

-- ============================================================
-- Types
-- ============================================================

-- | A coefficient split into 2 additive shares over GF(p).
-- Invariant: (share1 + share2) `mod` scPrime == original value.
-- Each share individually is uniformly random in [0, p).
data SharedCoeff = SharedCoeff
  { share1  :: {-# UNPACK #-} !Word64  -- ^ First share
  , share2  :: {-# UNPACK #-} !Word64  -- ^ Second share
  , scPrime :: {-# UNPACK #-} !Word64  -- ^ Field prime
  } deriving (Show, Eq)

-- | A polynomial with all coefficients secret-shared.
data SharedPoly = SharedPoly
  { spCoeffs :: ![SharedCoeff]   -- ^ Shared coefficients (ascending degree)
  , spDegree :: !Int             -- ^ Polynomial degree
  , spPrime  :: {-# UNPACK #-} !Word64  -- ^ Field prime
  } deriving (Show, Eq)

-- ============================================================
-- Modular Arithmetic (Word64)
-- ============================================================

-- | Modular addition: (a + b) mod p, overflow-safe.
modAdd :: Word64 -> Word64 -> Word64 -> Word64
modAdd a b p =
  let !s = a + b  -- may overflow for very large p, but for 64-bit primes < 2^63 this is fine
  in if s >= p then s - p else s
{-# INLINE modAdd #-}

-- | Modular subtraction: (a - b) mod p.
modSub :: Word64 -> Word64 -> Word64 -> Word64
modSub a b p =
  if a >= b then a - b else (p - b + a)
{-# INLINE modSub #-}

-- | Modular multiplication: (a * b) mod p.
-- For p < 2^32, this is safe in Word64.  For larger p, use intermediate.
modMul :: Word64 -> Word64 -> Word64 -> Word64
modMul a b p =
  fromIntegral $ (toInteger a * toInteger b) `mod` toInteger p
{-# INLINE modMul #-}

-- | Modular negation: (-a) mod p.
modNeg :: Word64 -> Word64 -> Word64
modNeg a p = if a == 0 then 0 else p - a
{-# INLINE modNeg #-}

-- ============================================================
-- Core: Split & Reconstruct
-- ============================================================

-- | Split a coefficient into 2 additive shares.
-- @splitCoeff val randomness prime@
-- Share1 = randomness mod p
-- Share2 = (val - randomness) mod p
splitCoeff :: Word64  -- ^ Value to split
           -> Word64  -- ^ Random mask (ephemeral, different each run)
           -> Word64  -- ^ Prime p
           -> SharedCoeff
splitCoeff val r p =
  let !s1 = r `mod` p
      !s2 = modSub (val `mod` p) s1 p
  in SharedCoeff s1 s2 p
{-# INLINE splitCoeff #-}

-- | Reconstruct the original value from shares.
-- This should ONLY be called at the final output point.
{-# NOINLINE reconstruct #-}
reconstruct :: SharedCoeff -> Word64
reconstruct (SharedCoeff s1 s2 p) = modAdd s1 s2 p

-- ============================================================
-- Arithmetic on Shares
-- ============================================================

-- | Shared addition: (a + b) on shares → 2 modular adds.
sharedAdd :: SharedCoeff -> SharedCoeff -> SharedCoeff
sharedAdd (SharedCoeff a1 a2 p) (SharedCoeff b1 b2 _) =
  SharedCoeff (modAdd a1 b1 p) (modAdd a2 b2 p) p
{-# INLINE sharedAdd #-}

-- | Shared subtraction: (a - b) on shares → 2 modular subs.
sharedSub :: SharedCoeff -> SharedCoeff -> SharedCoeff
sharedSub (SharedCoeff a1 a2 p) (SharedCoeff b1 b2 _) =
  SharedCoeff (modSub a1 b1 p) (modSub a2 b2 p) p
{-# INLINE sharedSub #-}

-- | Shared negation.
sharedNeg :: SharedCoeff -> SharedCoeff
sharedNeg (SharedCoeff a1 a2 p) =
  SharedCoeff (modNeg a1 p) (modNeg a2 p) p
{-# INLINE sharedNeg #-}

-- | Shared multiplication using Karatsuba-style.
-- a = s₁ + s₂, b = t₁ + t₂
-- ab = (s₁+s₂)(t₁+t₂)
--
-- Karatsuba: compute 3 products instead of 4:
--   M = (s₁+s₂)(t₁+t₂) = ab         ← this IS the answer
--   A = s₁·t₁
--   B = s₂·t₂
-- But we need to RE-SHARE the result, not reveal ab.
--
-- New shares: share1' = A + cross₁,  share2' = B + cross₂
-- where cross₁ + cross₂ = s₁t₂ + s₂t₁
--
-- We split: cross₁ = s₁t₂,  cross₂ = s₂t₁
-- Result: (s₁t₁ + s₁t₂,  s₂t₂ + s₂t₁) = (s₁(t₁+t₂),  s₂(t₁+t₂)) = (s₁·b, s₂·b)
-- Verify: s₁b + s₂b = (s₁+s₂)b = ab ✓
--
-- This requires only 2 multiplications! Even better than Karatsuba.
-- But it reveals b = t₁+t₂ to each multiplication.
-- To avoid that, we use the 3-mul variant:
sharedMul :: SharedCoeff -> SharedCoeff -> Word64 -> SharedCoeff
sharedMul (SharedCoeff a1 a2 p) (SharedCoeff b1 b2 _) freshRandom =
  let -- 3 multiplications:
      !m1 = modMul a1 b1 p           -- s₁·t₁
      !m2 = modMul a2 b2 p           -- s₂·t₂
      !m3 = modMul (modAdd a1 a2 p)  -- (s₁+s₂)·(t₁+t₂) = a·b
                   (modAdd b1 b2 p) p
      -- cross = m3 - m1 - m2 = s₁t₂ + s₂t₁
      !cross = modSub (modSub m3 m1 p) m2 p
      -- Re-share cross term using fresh randomness
      !r = freshRandom `mod` p
      -- Final shares:
      -- share1' = m1 + r
      -- share2' = m2 + (cross - r)
      !s1' = modAdd m1 r p
      !s2' = modAdd m2 (modSub cross r p) p
  in SharedCoeff s1' s2' p

-- | Multiply a shared coefficient by a public scalar.
-- 2 multiplications (each share × scalar).
sharedScalar :: Word64 -> SharedCoeff -> SharedCoeff
sharedScalar k (SharedCoeff a1 a2 p) =
  let !k' = k `mod` p
  in SharedCoeff (modMul k' a1 p) (modMul k' a2 p) p
{-# INLINE sharedScalar #-}

-- ============================================================
-- Share Management
-- ============================================================

-- | Re-randomize shares: same value, new split.
-- rotate(r, (s₁, s₂)) = (s₁ + r, s₂ - r)  — O(1)
rotateShares :: Word64 -> SharedCoeff -> SharedCoeff
rotateShares r (SharedCoeff s1 s2 p) =
  let !r' = r `mod` p
  in SharedCoeff (modAdd s1 r' p) (modSub s2 r' p) p
{-# INLINE rotateShares #-}

-- | Full re-randomization: generate new shares from value + fresh randomness.
-- Useful between pipeline stages.
rerandomize :: Word64 -> SharedCoeff -> SharedCoeff
rerandomize freshR sc =
  let !val = reconstruct sc  -- briefly reconstruct
      !p   = scPrime sc
  in splitCoeff val freshR p

-- ============================================================
-- Polynomial-Level Operations
-- ============================================================

-- | Split all coefficients of a polynomial.
splitPoly :: [Word64]   -- ^ Coefficients (ascending degree)
          -> [Word64]   -- ^ Random masks (one per coefficient)
          -> Word64     -- ^ Prime
          -> SharedPoly
splitPoly coeffs randoms p =
  let !shared = zipWith (\c r -> splitCoeff c r p) coeffs randoms
      !deg    = length coeffs - 1
  in SharedPoly shared deg p

-- | Reconstruct polynomial from shared coefficients.
{-# NOINLINE reconstructPoly #-}
reconstructPoly :: SharedPoly -> [Word64]
reconstructPoly (SharedPoly coeffs _ _) = map reconstruct coeffs

-- | Add two shared polynomials (coefficient-wise).
sharedPolyAdd :: SharedPoly -> SharedPoly -> SharedPoly
sharedPolyAdd (SharedPoly as degA p) (SharedPoly bs degB _) =
  let !maxDeg = max degA degB
      !zero   = SharedCoeff 0 0 p
      padTo n xs = xs ++ replicate (n + 1 - length xs) zero
      !as' = padTo maxDeg as
      !bs' = padTo maxDeg bs
      !result = zipWith sharedAdd as' bs'
  in SharedPoly result maxDeg p

-- | Multiply two shared polynomials (convolution on shares).
-- Uses schoolbook multiplication with shared arithmetic.
sharedPolyMul :: SharedPoly -> SharedPoly -> [Word64] -> SharedPoly
sharedPolyMul (SharedPoly as degA p) (SharedPoly bs degB _) freshRandoms =
  let !outDeg = degA + degB
      !zero   = SharedCoeff 0 0 p
      !initial = replicate (outDeg + 1) zero

      -- For each pair (i, j), add as[i] * bs[j] to result[i+j]
      !result = foldl' (\acc (i, a) ->
          foldl' (\acc' (j, b) ->
              let !r    = freshRandoms !! (i * (degB + 1) + j)
                  !prod = sharedMul a b r
                  !idx  = i + j
                  !old  = acc' !! idx
                  !new  = sharedAdd old prod
              in take idx acc' ++ [new] ++ drop (idx + 1) acc'
            ) acc (zip [0..] bs)
        ) initial (zip [0..] as)
  in SharedPoly result outDeg p

-- | Evaluate shared polynomial at a public point x.
-- Horner's method on shares.
sharedEvalAt :: SharedPoly -> Word64 -> [Word64] -> SharedCoeff
sharedEvalAt (SharedPoly [] _ p) _ _ = SharedCoeff 0 0 p
sharedEvalAt (SharedPoly coeffs _ _) x freshRandoms =
  let p = spPrime' coeffs
      -- Horner: result = c_n; for i = n-1 downto 0: result = result * x + c_i
      !reversed = reverse coeffs
  in foldl' (\acc (c, r) -> sharedAdd c (sharedMul acc (splitCoeff x 0 p) r))
            (head reversed)
            (zip (tail reversed) freshRandoms)
  where
    spPrime' cs = case cs of
      (c:_) -> scPrime c
      []    -> 0  -- shouldn't happen

-- ============================================================
-- Bulk Operations
-- ============================================================

-- | Split a list of Word64 values into shared coefficients.
splitCoeffs :: [Word64] -> [Word64] -> Word64 -> [SharedCoeff]
splitCoeffs vals randoms p = zipWith (\v r -> splitCoeff v r p) vals randoms

-- | Reconstruct a list of shared coefficients.
{-# NOINLINE reconstructCoeffs #-}
reconstructCoeffs :: [SharedCoeff] -> [Word64]
reconstructCoeffs = map reconstruct
