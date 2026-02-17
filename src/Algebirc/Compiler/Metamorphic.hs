module Algebirc.Compiler.Metamorphic
  ( obfuscateConstant
  , MetamorphicStrategy(..)
  ) where

import System.Random
import Data.Bits (xor)

-- | Available metamorphic strategies for constant obfuscation.
data MetamorphicStrategy
  = LinearIdentity          -- (x - k) + k
  | ModuloIdentity          -- (A * B + x) `mod` B
  | BitwiseXorIdentity      -- (x `xor` k) `xor` k
  | IntegralSumIdentity     -- foldl (+) k [1..n]
  | PowerMapApproxIdentity  -- floor (sqrt (x^2 + k))
  deriving (Show, Enum, Bounded)

-- | Obfuscate an integer constant into a complex Haskell expression string.
obfuscateConstant :: RandomGen g => Integer -> g -> (String, g)
obfuscateConstant val g =
  let (stratIdx, g1) = randomR (0, fromEnum (maxBound :: MetamorphicStrategy)) g
      strategy = toEnum stratIdx
  in applyStrategy strategy val g1

applyStrategy :: RandomGen g => MetamorphicStrategy -> Integer -> g -> (String, g)

-- Strategy 1: Linear Identity
-- val = (val - k) + k
applyStrategy LinearIdentity val g =
  let (k, g1) = randomR (-10000, 10000) g
      term1 = val - k
      expr = if k < 0 
                then "(" ++ show term1 ++ " - " ++ show (abs k) ++ ")"
                else "(" ++ show term1 ++ " + " ++ show k ++ ")"
  in (expr, g1)

-- Strategy 2: Modulo Identity
-- val = (A * B + val) `mod` B (where B > val)
applyStrategy ModuloIdentity val g =
  let (b, g1) = randomR (val + 1, val + 10000) g
      (a, g2) = randomR (1, 100) g1
      numerator = a * b + val
  in ("((" ++ show numerator ++ ") `mod` " ++ show b ++ ")", g2)

-- Strategy 3: Bitwise XOR Identity
-- val = (val `xor` k) `xor` k
applyStrategy BitwiseXorIdentity val g =
  let (k, g1) = randomR (1, 1000000) g
      masked = val `xor` k
  in ("((" ++ show masked ++ " `xor` " ++ show k ++ ") `xor` " ++ show k ++ ")", g1)

-- Strategy 4: Integral Sum Identity
-- val = result of foldl (+) k [1..n]
-- sum [1..n] = n(n+1)/2. So result = k + n(n+1)/2.
-- We fix n (e.g. 10 or random small number), calculate sum, then adjust k.
applyStrategy IntegralSumIdentity val g =
  let (n, g1) = randomR (5, 20 :: Integer) g
      seriesSum = (n * (n + 1)) `div` 2
      k = val - seriesSum
      kStr = if k < 0 then "(" ++ show k ++ ")" else show k
  in ("(foldl (+) " ++ kStr ++ " [1.." ++ show n ++ "])", g1)

-- Strategy 5: Power Map Approximation (Roots)
-- val = floor (sqrt (val^2 + k))
-- Constraint: val^2 <= val^2 + k < (val+1)^2
-- k < 2*val + 1
applyStrategy PowerMapApproxIdentity val g =
  let limit = 2 * abs val
      (k, g1) = randomR (0, limit) g
      discriminant = val * val + k
  in ("(floor (sqrt (" ++ show discriminant ++ ")))", g1)
