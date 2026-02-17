module Core.FiniteFieldSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Algebirc.Core.Types
import Algebirc.Core.FiniteField

-- | Generate a random non-zero field element in GF(257).
genFieldElement :: Gen FieldElement
genFieldElement = do
  v <- choose (0, 256)
  return (mkFieldElement v 257)

genNonZero :: Gen FieldElement
genNonZero = do
  v <- choose (1, 256)
  return (mkFieldElement v 257)

spec :: Spec
spec = do
  describe "FiniteField GF(257)" $ do
    -- Axiom 1: Closure
    it "addition is closed" $ property $
      forAll genFieldElement $ \a ->
        forAll genFieldElement $ \b ->
          let c = ffAdd a b
          in feValue c >= 0 && feValue c < 257

    it "multiplication is closed" $ property $
      forAll genFieldElement $ \a ->
        forAll genFieldElement $ \b ->
          let c = ffMul a b
          in feValue c >= 0 && feValue c < 257

    -- Axiom 2: Associativity
    it "addition is associative" $ property $
      forAll genFieldElement $ \a ->
        forAll genFieldElement $ \b ->
          forAll genFieldElement $ \c ->
            ffAdd (ffAdd a b) c == ffAdd a (ffAdd b c)

    it "multiplication is associative" $ property $
      forAll genFieldElement $ \a ->
        forAll genFieldElement $ \b ->
          forAll genFieldElement $ \c ->
            ffMul (ffMul a b) c == ffMul a (ffMul b c)

    -- Axiom 3: Commutativity
    it "addition is commutative" $ property $
      forAll genFieldElement $ \a ->
        forAll genFieldElement $ \b ->
          ffAdd a b == ffAdd b a

    it "multiplication is commutative" $ property $
      forAll genFieldElement $ \a ->
        forAll genFieldElement $ \b ->
          ffMul a b == ffMul b a

    -- Axiom 4: Identity
    it "additive identity" $ property $
      forAll genFieldElement $ \a ->
        ffAdd a (ffZero 257) == a

    it "multiplicative identity" $ property $
      forAll genFieldElement $ \a ->
        ffMul a (ffOne 257) == a

    -- Axiom 5: Inverse
    it "additive inverse" $ property $
      forAll genFieldElement $ \a ->
        ffAdd a (ffNeg a) == ffZero 257

    it "multiplicative inverse" $ property $
      forAll genNonZero $ \a ->
        case ffInv a of
          Right inv -> ffMul a inv == ffOne 257
          Left _    -> False  -- should not happen for non-zero

    -- Axiom 6: Distributivity
    it "multiplication distributes over addition" $ property $
      forAll genFieldElement $ \a ->
        forAll genFieldElement $ \b ->
          forAll genFieldElement $ \c ->
            ffMul a (ffAdd b c) == ffAdd (ffMul a b) (ffMul a c)

    -- Division
    it "division is inverse of multiplication" $ property $
      forAll genNonZero $ \a ->
        forAll genNonZero $ \b ->
          case ffDiv a b of
            Right q  -> ffMul q b == a
            Left _   -> False

    -- Exponentiation
    it "Fermat's little theorem: a^(p-1) = 1 for a /= 0" $ property $
      forAll genNonZero $ \a ->
        ffPow a 256 == ffOne 257

    -- Zero division
    it "division by zero returns error" $
      ffInv (ffZero 257) `shouldBe` Left DivisionByZero

    -- Extended GCD
    it "extended GCD correctness" $ property $
      \(Positive a) (Positive b) ->
        let (g, x, y) = extGcd (a :: Integer) b
        in a * x + b * y == g
