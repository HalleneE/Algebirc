-- |
-- Module      : Algebirc.Core.Matrix
-- Description : Polynomial Matrix Operations for Richelot Mappings
-- License     : MIT

module Algebirc.Core.Matrix
  ( PolyMatrix(..)
  , matMulPoly
  , matDet2x2
  , matDet3x3
  , FieldMatrix(..)
  , mkCauchyMatrix
  , matApplyField
  , matMulField
  , matInverseField
  ) where

import Algebirc.Core.Types (Poly)
import Algebirc.Geometry.HyperellipticCurve (polyAdd, polySub, polyMul)
import Algebirc.Geometry.EllipticCurve (modInv)
import qualified Data.Vector as V

-- | Matrix over a Field GF(p). Stored in row-major order.
data FieldMatrix = FieldMatrix
  { fieldRows :: !Int
  , fieldCols :: !Int
  , fieldData :: !(V.Vector Integer)
  } deriving (Show)

-- | Matrix multiplication over GF(p).
matMulField :: Integer -> FieldMatrix -> FieldMatrix -> FieldMatrix
matMulField p (FieldMatrix r1 c1 d1) (FieldMatrix r2 c2 d2)
  | c1 /= r2 = error "matMulField: dimension mismatch"
  | otherwise =
      let resData = V.generate (r1 * c2) $ \idx ->
            let i = idx `div` c2
                j = idx `mod` c2
                dotProduct k acc = (acc + (d1 V.! (i * c1 + k)) * (d2 V.! (k * c2 + j))) `mod` p
            in foldr dotProduct 0 [0 .. c1 - 1]
      in FieldMatrix r1 c2 resData

-- | Create a Cauchy MDS matrix A_{i,j} = 1 / (x_i + y_j) mod p.
-- Requires x_i, y_j to be distinct and x_i + y_j != 0.
mkCauchyMatrix :: Integer -> Int -> FieldMatrix
mkCauchyMatrix p n =
  let x = [0 .. fromIntegral n - 1]
      y = [fromIntegral n .. 2 * fromIntegral n - 1]
      resData = V.generate (n * n) $ \idx ->
        let i = idx `div` n
            j = idx `mod` n
            val = (x !! i + y !! j) `mod` p
        in modInv val p
  in FieldMatrix n n resData

-- | Apply matrix to vector: Y = M * X.
matApplyField :: Integer -> FieldMatrix -> [Integer] -> [Integer]
matApplyField p (FieldMatrix r c d) v
  | c /= length v = error "matApplyField: dimension mismatch"
  | otherwise =
      [ foldl (\acc k -> (acc + (d V.! (i * c + k)) * (v !! k)) `mod` p) 0 [0 .. c - 1]
      | i <- [0 .. r - 1] ]

-- | Compute matrix inverse over GF(p) using Gaussian elimination.
matInverseField :: Integer -> FieldMatrix -> Either String FieldMatrix
matInverseField p (FieldMatrix n n1 d)
  | n /= n1 = Left "Matrix must be square"
  | otherwise = 
      let -- Augment with identity
          aug = V.generate (n * 2 * n) $ \idx ->
            let r = idx `div` (2 * n)
                c = idx `mod` (2 * n)
            in if c < n 
               then d V.! (r * n + c)
               else if c - n == r then 1 else 0
          
          -- Gaussian elimination
          solve r currentAug
            | r == n = Right currentAug
            | otherwise =
                let -- Find pivot
                    pivotRow = head $ [ pr | pr <- [r .. n - 1]
                                     , (currentAug V.! (pr * 2 * n + r)) `mod` p /= 0 ] ++ [-1]
                in if pivotRow == -1 
                   then Left "Matrix is singular"
                   else 
                     let -- Swap rows
                         swapped = if pivotRow == r then currentAug else swapRows r pivotRow currentAug
                         -- Normalize pivot row
                         pivotVal = swapped V.! (r * 2 * n + r)
                         invPivot = modInv pivotVal p
                         normalized = V.generate (n * 2 * n) $ \idx ->
                           let row = idx `div` (2 * n)
                               col = idx `mod` (2 * n)
                           in if row == r 
                              then (swapped V.! idx * invPivot) `mod` p
                              else swapped V.! idx
                         -- Eliminate other rows
                         eliminated = V.generate (n * 2 * n) $ \idx ->
                           let row = idx `div` (2 * n)
                               col = idx `mod` (2 * n)
                           in if row /= r 
                              then let factor = normalized V.! (row * 2 * n + r)
                                   in (normalized V.! idx - factor * (normalized V.! (r * 2 * n + col))) `mod` p
                              else normalized V.! idx
                     in solve (r + 1) eliminated
                     
          swapRows r1 r2 vec = V.generate (n * 2 * n) $ \idx ->
            let r = idx `div` (2 * n)
                c = idx `mod` (2 * n)
            in if r == r1 then vec V.! (r2 * 2 * n + c)
               else if r == r2 then vec V.! (r1 * 2 * n + c)
               else vec V.! idx
               
      in case solve 0 aug of
           Left err -> Left err
           Right resAug -> 
             let invData = V.generate (n * n) $ \idx ->
                   let r = idx `div` n
                       c = idx `mod` n
                   in resAug V.! (r * 2 * n + (c + n))
             in Right $ FieldMatrix n n invData

-- | Matrix over GF(p)[x]. Stored in row-major order.
data PolyMatrix = PolyMatrix
  { matRows :: !Int
  , matCols :: !Int
  , matData :: !(V.Vector Poly)
  } deriving (Show)

-- | Look up an element at (row, col) (0-indexed).
matGet :: PolyMatrix -> Int -> Int -> Poly
matGet (PolyMatrix _ c d) row col = d V.! (row * c + col)

-- | Polynomial Matrix Multiplication over GF(p)[x]
matMulPoly :: Integer -> PolyMatrix -> PolyMatrix -> PolyMatrix
matMulPoly p m1@(PolyMatrix r1 c1 _) m2@(PolyMatrix r2 c2 _)
  | c1 /= r2 = error "matMulPoly: dimension mismatch"
  | otherwise =
      let resData = V.generate (r1 * c2) $ \idx ->
            let i = idx `div` c2
                j = idx `mod` c2
                -- dot product of i-th row of m1 and j-th col of m2
                terms = [ polyMul p (matGet m1 i k) (matGet m2 k j) | k <- [0 .. c1 - 1] ]
            in foldl (polyAdd p) (V.singleton 0) terms
      in PolyMatrix r1 c2 resData
      
-- | Determinant of a 2x2 Polynomial Matrix over GF(p)[x]
matDet2x2 :: Integer -> PolyMatrix -> Poly
matDet2x2 p m@(PolyMatrix 2 2 _) =
  let a00 = matGet m 0 0
      a01 = matGet m 0 1
      a10 = matGet m 1 0
      a11 = matGet m 1 1
  in polySub p (polyMul p a00 a11) (polyMul p a01 a10)
matDet2x2 _ _ = error "matDet2x2: must be exactly 2x2"

-- | Determinant of a 3x3 Polynomial Matrix over GF(p)[x]
matDet3x3 :: Integer -> PolyMatrix -> Poly
matDet3x3 p m@(PolyMatrix 3 3 _) =
  let a = matGet m 0 0; b = matGet m 0 1; c = matGet m 0 2
      d = matGet m 1 0; e = matGet m 1 1; f = matGet m 1 2
      g = matGet m 2 0; h = matGet m 2 1; i = matGet m 2 2
      
      term1 = polyMul p a (polySub p (polyMul p e i) (polyMul p f h))
      term2 = polyMul p b (polySub p (polyMul p d i) (polyMul p f g))
      term3 = polyMul p c (polySub p (polyMul p d h) (polyMul p e g))
  in polyAdd p (polySub p term1 term2) term3
matDet3x3 _ _ = error "matDet3x3: must be exactly 3x3"
