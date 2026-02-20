{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve
import Algebirc.Geometry.Isogeny
import Algebirc.Geometry.Hardness

-- | Helper to generate arbitrary field elements mod 257
newtype FieldElem257 = FieldElem257 Integer deriving (Show, Eq)

instance Arbitrary FieldElem257 where
  arbitrary = FieldElem257 <$> choose (0, 256)

-- | Generator for a valid, non-singular elliptic curve over GF(257)
-- It generates random a, b and ensures the discriminant is non-zero.
newtype ValidCurve257 = ValidCurve257 EllipticCurve

instance Show ValidCurve257 where
  show (ValidCurve257 (EllipticCurve a b p)) =
    "EllipticCurve a=" ++ show a ++ " b=" ++ show b ++ " p=" ++ show p

instance Arbitrary ValidCurve257 where
  arbitrary = do
    a <- choose (0, 256)
    b <- choose (0, 256)
    let p = 257
        disc = (4 * a * a * a + 27 * b * b) `mod` p
    if disc == 0
      then arbitrary  -- retry if singular
      else return (ValidCurve257 (EllipticCurve a b p))

-- | Generator for an isogeny walk steps (CSIDH-like parameter)
-- List of (seed, ell) where ell is a small prime.
newtype WalkSteps = WalkSteps [(Integer, Int)] deriving Show

instance Arbitrary WalkSteps where
  arbitrary = do
    -- Let's use small valid primes for our isogenies
    let primes = [3, 5, 7, 11]
    -- Generate length 1 to 5
    n <- choose (1, 5)
    steps <- vectorOf n $ do
      p <- elements primes
      seed <- choose (1, 256)
      return (seed, p)
    return (WalkSteps steps)

-- | Generate lists of integers for obfuscation roundtrip
newtype CoeffList = CoeffList [Integer] deriving Show

instance Arbitrary CoeffList where
  arbitrary = do
    n <- choose (2, 20)
    -- Must be mod 257 so we keep them in field range
    xs <- vectorOf n $ choose (0, 256)
    return (CoeffList xs)

-- | Security parameters for hardness testing
testParams ec = customParams ec [3, 5, 7, 11] 3

-- | Generator for a non-trivial obfuscation setup
-- Generates a curve, original coeffs, and a secret key such that pkCurve /= ec
data NonTrivialObfuscation = NonTrivialObfuscation ValidCurve257 CoeffList Integer 

instance Show NonTrivialObfuscation where
  show (NonTrivialObfuscation ec coeffs seed) =
    "NonTrivialObfuscation (" ++ show ec ++ ") (" ++ show coeffs ++ ") seed=" ++ show seed

instance Arbitrary NonTrivialObfuscation where
  arbitrary = do
    -- We wrap the core logic in a function we can retry
    let tryGenerate stateRounds = do
          vc@(ValidCurve257 ec) <- arbitrary
          n <- choose (2, 20)
          xs <- vectorOf n $ choose (0, 256)
          let cl = CoeffList xs
          let params = testParams ec
          
          -- Try 50 random seeds on this specific curve
          let findOnCurve 0 = return Nothing
              findOnCurve tries = do
                seed <- choose (1, 1000000)
                let sk = generateSecretKey params seed
                    pk = derivePublicKey sk
                if pkCurve pk /= ec 
                  then return (Just seed)
                  else findOnCurve (tries - 1)
                  
          res <- findOnCurve (50 :: Int)
          case res of
            Just goodSeed -> return (NonTrivialObfuscation vc cl goodSeed)
            Nothing       -> tryGenerate (stateRounds - 1)
            
    -- We assume within a few curve re-rolls we'll find one with a rich graph
    tryGenerate (100 :: Int)

spec :: Spec
spec = describe "Algebirc.Geometry Properties (QuickCheck)" $ do
  
  describe "Isogeny Properties" $ do
    it "prop_isogenyPreservesOrder: curveOrder(E) == curveOrder(applyIsogeny(E))" $
      property $ \(ValidCurve257 ec) (WalkSteps steps) ->
        let ec' = isogenyWalk ec steps
        in curveOrder ec === curveOrder ec'

  describe "Hardness Scheme Properties" $ do
    it "prop_roundtrip: deobfuscate(obfuscate(coeffs)) == coeffs" $
      property $ \(ValidCurve257 ec) (CoeffList original) seed ->
        let params = testParams ec
            sk = generateSecretKey params seed
            (obfuscated, wit) = obfuscateWithCSIDH sk original
            recovered = deobfuscateWithCSIDH sk obfuscated
        in (recovered === original) .&&.
           counterexample "GAIP Witness must be valid" (verifyWitness wit)
           
    it "prop_obfuscationChangesValues: obfuscating modifies the polynomial (unless trivially E₀=Eₖ)" $
      property $ \(NonTrivialObfuscation (ValidCurve257 ec) (CoeffList original) seed) ->
        let params = testParams ec
            sk = generateSecretKey params seed
            (obfuscated, _) = obfuscateWithCSIDH sk original
        in obfuscated =/= original
