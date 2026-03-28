{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- |
-- Module      : Algebirc.Core.U256FFI
-- Description : FFI bindings to C/Assembly Constant-Time backend
-- License     : MIT
--
-- This module acts as the orchestration layer binding Haskell's memory
-- safely to the hardware-constant-time C core. 

module Algebirc.Core.U256FFI 
  ( U256(..)
  , toU256
  , fromU256
  , c_addMod256
  , c_subMod256
  , c_select256
  , c_eq256
  , c_mont_mul_ct
  , c_modpow_ct
  , c_modinv_ct
  ) where

import Data.Word
import Data.Bits
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

-- | We still use the strictly unboxed Haskell representation for memory footprint.
data U256 = U256
  {-# UNPACK #-} !Word64 -- l0
  {-# UNPACK #-} !Word64 -- l1
  {-# UNPACK #-} !Word64 -- l2
  {-# UNPACK #-} !Word64 -- l3
  deriving (Eq, Show)

toU256 :: Integer -> U256
toU256 n =
  let mask64 = 0xFFFFFFFFFFFFFFFF
      l0 = fromIntegral $ n .&. mask64
      l1 = fromIntegral $ (n `shiftR` 64) .&. mask64
      l2 = fromIntegral $ (n `shiftR` 128) .&. mask64
      l3 = fromIntegral $ (n `shiftR` 192) .&. mask64
  in U256 l0 l1 l2 l3

fromU256 :: U256 -> Integer
fromU256 (U256 l0 l1 l2 l3) =
  toInteger l0 .|.
  (toInteger l1 `shiftL` 64) .|.
  (toInteger l2 `shiftL` 128) .|.
  (toInteger l3 `shiftL` 192)

-- Helper to safely pass U256 to C
withU256 :: U256 -> (Ptr Word64 -> IO a) -> IO a
withU256 (U256 w0 w1 w2 w3) action =
  allocaBytes 32 $ \ptr -> do
    pokeElemOff ptr 0 w0
    pokeElemOff ptr 1 w1
    pokeElemOff ptr 2 w2
    pokeElemOff ptr 3 w3
    action ptr

-- Helper to read U256 from C
peekU256 :: Ptr Word64 -> IO U256
peekU256 ptr = do
  w0 <- peekElemOff ptr 0
  w1 <- peekElemOff ptr 1
  w2 <- peekElemOff ptr 2
  w3 <- peekElemOff ptr 3
  return $ U256 w0 w1 w2 w3

-- ============================================================
-- FFI Imports
-- ============================================================

-- void u256_modadd_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m);
foreign import ccall unsafe "u256_modadd_ct"
  c_u256_modadd_ct :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

-- void u256_modsub_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m);
foreign import ccall unsafe "u256_modsub_ct"
  c_u256_modsub_ct :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

-- void u256_select_ct(u256_t r, uint64_t cond, const u256_t a, const u256_t b);
foreign import ccall unsafe "u256_select_ct"
  c_u256_select_ct :: Ptr Word64 -> Word64 -> Ptr Word64 -> Ptr Word64 -> IO ()

-- uint64_t u256_eq_ct(const u256_t a, const u256_t b);
foreign import ccall unsafe "u256_eq_ct"
  c_u256_eq_ct :: Ptr Word64 -> Ptr Word64 -> IO Word64

-- void u256_mont_mul_ct(u256_t r, const u256_t a, const u256_t b, const u256_t m, uint64_t m0_inv);
foreign import ccall unsafe "u256_mont_mul_ct"
  c_u256_mont_mul_ct :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Word64 -> IO ()

-- void u256_modpow_ct(u256_t r, const u256_t base_mont, const u256_t exp, const u256_t m, uint64_t m0_inv, const u256_t mont_one);
foreign import ccall unsafe "u256_modpow_ct"
  c_u256_modpow_ct :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

-- void u256_modinv_ct(u256_t r, const u256_t a_mont, const u256_t m, uint64_t m0_inv, const u256_t mont_one);
foreign import ccall unsafe "u256_modinv_ct"
  c_u256_modinv_ct :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> Word64 -> Ptr Word64 -> IO ()

-- ============================================================
-- Haskell Wrappers (Pure)
-- ============================================================

c_addMod256 :: U256 -> U256 -> U256 -> U256
c_addMod256 a b m = unsafePerformIO $
  allocaBytes 32 $ \rPtr ->
  withU256 a $ \aPtr ->
  withU256 b $ \bPtr ->
  withU256 m $ \mPtr -> do
    c_u256_modadd_ct rPtr aPtr bPtr mPtr
    peekU256 rPtr

c_subMod256 :: U256 -> U256 -> U256 -> U256
c_subMod256 a b m = unsafePerformIO $
  allocaBytes 32 $ \rPtr ->
  withU256 a $ \aPtr ->
  withU256 b $ \bPtr ->
  withU256 m $ \mPtr -> do
    c_u256_modsub_ct rPtr aPtr bPtr mPtr
    peekU256 rPtr

c_select256 :: Word64 -> U256 -> U256 -> U256
c_select256 cond a b = unsafePerformIO $
  allocaBytes 32 $ \rPtr ->
  withU256 a $ \aPtr ->
  withU256 b $ \bPtr -> do
    c_u256_select_ct rPtr cond aPtr bPtr
    peekU256 rPtr

c_eq256 :: U256 -> U256 -> Word64
c_eq256 a b = unsafePerformIO $
  withU256 a $ \aPtr ->
  withU256 b $ \bPtr -> do
    c_u256_eq_ct aPtr bPtr

c_mont_mul_ct :: U256 -> U256 -> U256 -> Word64 -> U256
c_mont_mul_ct a b m m0_inv = unsafePerformIO $
  allocaBytes 32 $ \rPtr ->
  withU256 a $ \aPtr ->
  withU256 b $ \bPtr ->
  withU256 m $ \mPtr -> do
    c_u256_mont_mul_ct rPtr aPtr bPtr mPtr m0_inv
    peekU256 rPtr

c_modpow_ct :: U256 -> U256 -> U256 -> Word64 -> U256 -> U256
c_modpow_ct base_mont ex m m0_inv mont_one = unsafePerformIO $
  allocaBytes 32 $ \rPtr ->
  withU256 base_mont $ \basePtr ->
  withU256 ex $ \exPtr ->
  withU256 m $ \mPtr ->
  withU256 mont_one $ \onePtr -> do
    c_u256_modpow_ct rPtr basePtr exPtr mPtr m0_inv onePtr
    peekU256 rPtr

c_modinv_ct :: U256 -> U256 -> Word64 -> U256 -> U256
c_modinv_ct a_mont m m0_inv mont_one = unsafePerformIO $
  allocaBytes 32 $ \rPtr ->
  withU256 a_mont $ \aPtr ->
  withU256 m $ \mPtr ->
  withU256 mont_one $ \onePtr -> do
    c_u256_modinv_ct rPtr aPtr mPtr m0_inv onePtr
    peekU256 rPtr
