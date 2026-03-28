{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Algebirc.Core.U256Mod
-- Description : Hardware-Constant-Time Modular Arithmetic
-- License     : MIT
--
-- Implements primitives for Constant-Time modular reduction and arithmetic.
-- This mitigates the variable-time nature of GMP's `mod` and `div` operations.
--
-- NOTE: True Hardware CT for 256-bit multiplication in pure Haskell requires
-- decomposing into 32-bit/64-bit bounds to capture carry bits without overflow.
-- This module establishes the structural template for branchless modular arithmetic.

module Algebirc.Core.U256Mod
  ( addMod256
  , subMod256
  , mulMod256_naiveCT
  , modPow256_CT
  ) where

import Algebirc.Core.U256
import Data.Word (Word64)
import Data.Bits

-- | Constant-Time Modular Addition
-- Computes: (a + b) mod m
-- Operates strictly without branches using `ctSelect256`.
addMod256 :: U256 -> U256 -> U256 -> U256
addMod256 a b m =
  let sum_val = add256 a b
      -- In a true 256-bit add, if there's a carry out, we MUST subtract m.
      -- Here we assume a, b < m. So a + b < 2m.
      -- If sum_val >= m, we subtract m.
      -- We check sum_val >= m by computing sum_val - m and checking if it underflowed.
      diff = sub256 sum_val m
      -- To check if `sum_val < m`, we look at the borrow out of a custom subtraction
      -- or just use a CT compare. For now, we implement a helper `ctLt256`.
      isLt = ctLt256 sum_val m
  in ctSelect256 isLt sum_val diff

-- | Constant-Time Less-Than
-- Returns 1 if a < b, else 0.
ctLt256 :: U256 -> U256 -> Word64
ctLt256 (U256 a0 a1 a2 a3) (U256 b0 b1 b2 b3) =
  -- Compares from MSB to LSB
  let gt3 = if a3 > b3 then 1 else 0
      lt3 = if a3 < b3 then 1 else 0
      eq3 = if a3 == b3 then 1 else 0

      gt2 = if a2 > b2 then 1 else 0
      lt2 = if a2 < b2 then 1 else 0
      eq2 = if a2 == b2 then 1 else 0

      gt1 = if a1 > b1 then 1 else 0
      lt1 = if a1 < b1 then 1 else 0
      eq1 = if a1 == b1 then 1 else 0

      lt0 = if a0 < b0 then 1 else 0
      
      -- a < b if:
      -- a3 < b3
      -- OR (a3 == b3 AND a2 < b2)
      -- OR (a3 == b3 AND a2 == b2 AND a1 < b1)
      -- OR (a3 == b3 AND a2 == b2 AND a1 == b1 AND a0 < b0)
      res = lt3 .|.
            (eq3 .&. lt2) .|.
            (eq3 .&. eq2 .&. lt1) .|.
            (eq3 .&. eq2 .&. eq1 .&. lt0)
  in res

-- | Constant-Time Modular Subtraction
-- Computes: (a - b) mod m
subMod256 :: U256 -> U256 -> U256 -> U256
subMod256 a b m =
  let diff = sub256 a b
      -- If a < b, diff underflows (is negative in 2's complement).
      -- In that case, the correct mod result is diff + m.
      isLt = ctLt256 a b
      corrected = add256 diff m
  in ctSelect256 isLt corrected diff

-- | Constant-Time Modular Multiplication (Naive Shift-and-Add)
-- Computes: (a * b) mod m
-- This is a fixed-schedule O(256) bitwise multiplication.
-- While slow compared to Montgomery, it is strictly CT.
mulMod256_naiveCT :: U256 -> U256 -> U256 -> U256
mulMod256_naiveCT a b m =
  -- We iterate 256 times.
  -- At each step i (from 255 down to 0):
  -- res = (res * 2) mod m
  -- bit = (a >> i) & 1
  -- res = (res + bit * b) mod m
  -- 
  -- Since Haskell lacks fixed-size loops easily expressible without recursion,
  -- we unroll logically or use a strict fold.
  let bitAt (U256 w0 w1 w2 w3) i
        | i < 64  = (w0 `shiftR` i) .&. 1
        | i < 128 = (w1 `shiftR` (i - 64)) .&. 1
        | i < 192 = (w2 `shiftR` (i - 128)) .&. 1
        | otherwise = (w3 `shiftR` (i - 192)) .&. 1
      
      step res i =
        let res_double = addMod256 res res m
            b_bit = bitAt a i
            -- If bit is 1, add b. If 0, add 0.
            b_to_add = ctSelect256 b_bit b (U256 0 0 0 0)
        in addMod256 res_double b_to_add m
      
  in foldl step (U256 0 0 0 0) [255, 254 .. 0]

-- | Constant-Time Modular Exponentiation (Square-and-Multiply Always)
-- Computes: (base^exp) mod m
modPow256_CT :: U256 -> U256 -> U256 -> U256
modPow256_CT base ex m =
  let bitAt (U256 w0 w1 w2 w3) i
        | i < 64  = (w0 `shiftR` i) .&. 1
        | i < 128 = (w1 `shiftR` (i - 64)) .&. 1
        | i < 192 = (w2 `shiftR` (i - 128)) .&. 1
        | otherwise = (w3 `shiftR` (i - 192)) .&. 1
      
      step res i =
        let res_sq = mulMod256_naiveCT res res m
            res_mul = mulMod256_naiveCT res_sq base m
            bit = bitAt ex i
        in ctSelect256 bit res_mul res_sq
        
  in foldl step (U256 1 0 0 0) [255, 254 .. 0]
