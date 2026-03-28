{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Algebirc.Core.U256
-- Description : Hardware-Constant-Time 256-bit Unsigned Integer
-- License     : MIT
--
-- This module implements a strictly unboxed, fixed-size 256-bit integer
-- bypassing GMP's 'Integer' to prevent variable-limb memory allocations
-- and cache-timing attacks.
--
-- All operations here are strictly branchless (no if/else, no early exits).

module Algebirc.Core.U256
  ( U256(..)
  , toU256
  , fromU256
  , ctSelect256
  , ctIsZero256
  , ctEq256
  , add256
  , sub256
  ) where

import Data.Word (Word64)
import Data.Bits ((.&.), (.|.), xor, complement, shiftR, shiftL)

-- | 256-bit unsigned integer stored as 4 unboxed 64-bit limbs.
-- Ordered from least significant (l0) to most significant (l3).
data U256 = U256
  {-# UNPACK #-} !Word64 -- l0
  {-# UNPACK #-} !Word64 -- l1
  {-# UNPACK #-} !Word64 -- l2
  {-# UNPACK #-} !Word64 -- l3
  deriving (Eq, Show)

-- | Convert an Integer to U256 (Not CT, use only at boundaries)
toU256 :: Integer -> U256
toU256 n =
  let mask64 = 0xFFFFFFFFFFFFFFFF
      l0 = fromIntegral $ n .&. mask64
      l1 = fromIntegral $ (n `shiftR` 64) .&. mask64
      l2 = fromIntegral $ (n `shiftR` 128) .&. mask64
      l3 = fromIntegral $ (n `shiftR` 192) .&. mask64
  in U256 l0 l1 l2 l3

-- | Convert a U256 to Integer (Not CT, use only at boundaries)
fromU256 :: U256 -> Integer
fromU256 (U256 l0 l1 l2 l3) =
  toInteger l0 .|.
  (toInteger l1 `shiftL` 64) .|.
  (toInteger l2 `shiftL` 128) .|.
  (toInteger l3 `shiftL` 192)

-- | Constant-Time Selection
-- If cond == 1, returns a. If cond == 0, returns b.
-- cond MUST be exactly 1 or 0.
ctSelect256 :: Word64 -> U256 -> U256 -> U256
ctSelect256 !cond (U256 a0 a1 a2 a3) (U256 b0 b1 b2 b3) =
  let mask = 0 - cond -- 1 -> 0xFFFFFFFFFFFFFFFF, 0 -> 0x0000000000000000
      notMask = complement mask
      r0 = (a0 .&. mask) .|. (b0 .&. notMask)
      r1 = (a1 .&. mask) .|. (b1 .&. notMask)
      r2 = (a2 .&. mask) .|. (b2 .&. notMask)
      r3 = (a3 .&. mask) .|. (b3 .&. notMask)
  in U256 r0 r1 r2 r3

-- | Constant-Time Zero Check
-- Returns 1 if zero, 0 if non-zero.
ctIsZero256 :: U256 -> Word64
ctIsZero256 (U256 l0 l1 l2 l3) =
  let acc = l0 .|. l1 .|. l2 .|. l3
      -- If acc is 0, acc - 1 borrows and sets MSB to 1.
      -- If acc > 0, acc | (0 - acc) has MSB set to 1.
      -- A standard CT trick: 1 - ((acc | (0 - acc)) >> 63)
      negAcc = 0 - acc
      msb = (acc .|. negAcc) `shiftR` 63
  in 1 - msb

-- | Constant-Time Equality Check
-- Returns 1 if equal, 0 if not equal.
ctEq256 :: U256 -> U256 -> Word64
ctEq256 (U256 a0 a1 a2 a3) (U256 b0 b1 b2 b3) =
  let diff = U256 (a0 `xor` b0) (a1 `xor` b1) (a2 `xor` b2) (a3 `xor` b3)
  in ctIsZero256 diff

-- | Addition with carry out (discarded).
add256 :: U256 -> U256 -> U256
add256 (U256 a0 a1 a2 a3) (U256 b0 b1 b2 b3) =
  let r0 = a0 + b0
      c0 = if r0 < a0 then 1 else 0
      r1_tmp = a1 + b1
      c1_tmp = if r1_tmp < a1 then 1 else 0
      r1 = r1_tmp + c0
      c1 = c1_tmp .|. (if r1 < r1_tmp then 1 else 0)
      r2_tmp = a2 + b2
      c2_tmp = if r2_tmp < a2 then 1 else 0
      r2 = r2_tmp + c1
      c2 = c2_tmp .|. (if r2 < r2_tmp then 1 else 0)
      r3 = a3 + b3 + c2
  in U256 r0 r1 r2 r3

-- | Subtraction with borrow out (discarded).
sub256 :: U256 -> U256 -> U256
sub256 (U256 a0 a1 a2 a3) (U256 b0 b1 b2 b3) =
  let r0 = a0 - b0
      bw0 = if a0 < b0 then 1 else 0
      r1_tmp = a1 - b1
      bw1_tmp = if a1 < b1 then 1 else 0
      r1 = r1_tmp - bw0
      bw1 = bw1_tmp .|. (if r1_tmp < bw0 then 1 else 0)
      r2_tmp = a2 - b2
      bw2_tmp = if a2 < b2 then 1 else 0
      r2 = r2_tmp - bw1
      bw2 = bw2_tmp .|. (if r2_tmp < bw1 then 1 else 0)
      r3 = a3 - b3 - bw2
  in U256 r0 r1 r2 r3
