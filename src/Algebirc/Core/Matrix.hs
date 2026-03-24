-- |
-- Module      : Algebirc.Core.Matrix
-- Description : Polynomial Matrix Operations for Richelot Mappings
-- License     : MIT

module Algebirc.Core.Matrix
  ( PolyMatrix(..)
  , matMulPoly
  , matDet2x2
  , matDet3x3
  ) where

import Algebirc.Core.Types (Poly)
import Algebirc.Geometry.HyperellipticCurve (polyAdd, polySub, polyMul)
import qualified Data.Vector as V

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
