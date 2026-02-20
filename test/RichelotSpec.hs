{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RichelotSpec (spec) where

import Prelude

import Test.Hspec
import Test.QuickCheck

import Algebirc.Geometry.HyperellipticCurve
import Algebirc.Geometry.RichelotIsogeny
import Algebirc.Core.Types

-- ============================================================
-- Custom Generators
-- ============================================================

-- | A valid squarefree sextic polynomial over GF(257) that factors into 3 distinct quadratics.
-- Generating it backwards (from roots) guarantees it meets the Richelot prerequisites
-- and prevents infinite loops / high discard rates during testing.
newtype SquarefreeSextic = SquarefreeSextic [Integer]
  deriving (Show)

instance Arbitrary SquarefreeSextic where
  arbitrary = do
    -- We want 6 distinct roots to guarantee 3 distinct, squarefree quadratic factors
    let p = 257
    let allElements = [0 .. p - 1]
    
    -- Pick 6 unique roots
    roots <- vectorOf 6 (elements allElements) `suchThat` (\rs -> length (canonicalize rs) == 6)
    
    -- Group into pairs to form quadratics
    let (r1:r2:r3:r4:r5:r6:_) = roots
    let g1 = buildQuad p r1 r2
    let g2 = buildQuad p r3 r4
    let g3 = buildQuad p r5 r6
    
    -- f(x) = G1 * G2 * G3
    let f = polyMul p (polyMul p g1 g2) g3
    
    return (SquarefreeSextic f)
    where
      -- Remove duplicates to check uniqueness
      canonicalize [] = []
      canonicalize (x:xs) = x : canonicalize (filter (/= x) xs)
      
      -- Helper to build (x-r1)(x-r2) = x^2 - (r1+r2)x + r1*r2
      buildQuad p r1 r2 = 
        let c0 = (r1 * r2) `mod` p
            c1 = ((p - r1 - r2) `mod` p + p) `mod` p
        in [c0, c1, 1]

-- ============================================================
-- Properties
-- ============================================================

-- | Property 1: factorSextic correctly splits f(x) into G1*G2*G3 = f(x)
prop_factorSexticCorrect :: SquarefreeSextic -> Property
prop_factorSexticCorrect (SquarefreeSextic f) =
  let p = 257
      (g1, g2, g3) = factorSextic p f
      product12 = polyMul p g1 g2
      reconstructed = polyMul p product12 g3
  in reconstructed === f

-- | Property 2: Applying Richelot isogeny yields a valid codomain Jacobian
-- i.e., discriminant != 0 (non-singular curve) and valid Igusa Invariants
prop_richelotCodomainValid :: SquarefreeSextic -> Property
prop_richelotCodomainValid (SquarefreeSextic f) =
  let p = 257
      -- Apply Richelot Step to get the dual curve C'
      f' = richelotStep p f
  in case mkHyperCurve f' p of
       Left err -> counterexample ("mkHyperCurve failed: " ++ err ++ "\nf': " ++ show f') False
       Right hc' ->
         let disc = hyperDiscriminant hc'
             IgusaInvariants j2 j4 j6 j10 = igusaInvariants hc'
         in counterexample ("Codomain polynomial: " ++ show f') $
              (disc /= 0)                             -- Must be non-singular
              .&&. (j2 /= 0 || j4 /= 0 || j6 /= 0 || j10 /= 0) -- Must have non-trivial invariants

-- | Property 3: Structural Proxy for Dual Isogeny Consistency
-- Since explicit divisor evaluation `richelotEval(D)` is not yet implemented,
-- we verify that the equations commute properly through the transport layer.
-- TODO: Upgrade to rigorous algebraic test (phi . phi_hat == [2]) once divisor eval is available.
prop_transportRoundtripProxy :: SquarefreeSextic -> [Integer] -> Property
prop_transportRoundtripProxy (SquarefreeSextic f) rawPayload =
  -- Ensure payload isn't empty to make the test meaningful
  not (null rawPayload) ==>
  let p = 257
      payload = map (`mod` p) rawPayload
      hc = case mkHyperCurve f p of
             Right c -> c
             Left _ -> error "Invalid base curve generated"
             
      walkSteps = 3
      transported = richelotTransport hc walkSteps payload
      recovered = richelotInverseTransport hc walkSteps transported
  in recovered === payload

-- ============================================================
-- Spec Driver
-- ============================================================

spec :: Spec
spec = describe "Algebirc.Geometry.RichelotIsogeny Properties (QuickCheck)" $ do
  
  describe "Sextic Factorization" $ do
    it "factorSextic: G1(x) * G2(x) * G3(x) == f(x)" $
      property prop_factorSexticCorrect

  describe "Codomain Correctness" $ do
    it "richelotStep produces a valid, non-singular Jacobian (Discriminant /= 0)" $
      property prop_richelotCodomainValid

  describe "Dual Isogeny Consistency (Structural Proxy)" $ do
    it "TODO: richelotEval(D) is missing. Using transport roundtrip as structural proxy for [2]-map." $
      property prop_transportRoundtripProxy
