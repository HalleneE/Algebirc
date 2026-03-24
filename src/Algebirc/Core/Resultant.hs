-- |
-- Module      : Algebirc.Core.Resultant
-- Description : Resultant Engine (Subresultant PRS / Euclidean Reduction)
-- License     : MIT
--
-- = Resultant Calculation over GF(p)
-- Computes the Exact Resultant of two polynomials A and B over a finite field GF(p).
-- Uses recursive polynomial pseudo-division.

module Algebirc.Core.Resultant
  ( polyResultant
  ) where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve (modPow)
import Algebirc.Geometry.HyperellipticCurve (polyMod, polyDeg, polyLeadCoeff)
import qualified Data.Vector as V

-- | Compute the Resultant of two polynomials A and B over GF(p).
-- Uses the Euclidean algorithm approach for fields (since we are in GF(p)).
-- Res(A, B) = (-1)^(deg A * deg B) * lc(B)^(deg A - deg R) * Res(B, R)
-- where R = A mod B.
polyResultant :: Integer -> Poly -> Poly -> Integer
polyResultant p a b
  | polyDeg a < polyDeg b = 
      let sign = if (polyDeg a * polyDeg b) `mod` 2 == 1 then -1 else 1
      in ((sign * polyResultant p b a) `mod` p + p) `mod` p
  | polyDeg b == -1 = 0
  | polyDeg b == 0  = modPow (polyLeadCoeff b) (fromIntegral $ polyDeg a) p
  | otherwise =
      let r = polyMod p a b
          degA = polyDeg a
          degB = polyDeg b
          degR = polyDeg r
          lcB = polyLeadCoeff b
          sign = if (degA * degB) `mod` 2 == 1 then -1 else 1
          -- factor1 = lc(B)^(deg A - deg R)
          factor1 = modPow lcB (fromIntegral $ degA - degR) p
          resBR = polyResultant p b r
      in (((sign * factor1) `mod` p * resBR) `mod` p + p) `mod` p
