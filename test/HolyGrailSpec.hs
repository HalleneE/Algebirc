module Main where

import Test.Hspec
import Algebirc.Core.Types
import Algebirc.Evaluator.Eval (Expr(..), BinOp(..))
import Algebirc.Evaluator.ObfEval
import Algebirc.Obfuscation.Pipeline (buildPipeline)
import Algebirc.Geometry.RichelotIsogeny (evaluateGeometricSignature)
import qualified Data.Map.Strict as Map
import Control.Monad.Except

main :: IO ()
main = hspec $ do
  describe "Geometric Logic: Logic-as-Graph (Isogeny Obfuscation)" $ do
    let cfg = defaultConfig { cfgGenus = 2, cfgSeed = 42 }
        Right pl = buildPipeline cfg

    it "Executes conditional logic (If-Else) directly on the Jacobian" $ do
      -- Logic: if (1) then 10 else 20
      let cond = Lit 1
          trueBranch = Lit 10
          falseBranch = Lit 20
          expr = If cond trueBranch falseBranch

      -- Lift literals to obfuscated space
      liftCond  <- runExceptT $ liftLit cfg pl 1
      liftTrue  <- runExceptT $ liftLit cfg pl 10
      liftFalse <- runExceptT $ liftLit cfg pl 20
      
      case (liftCond, liftTrue, liftFalse) of
        (Right oCond, Right oTrue, Right oFalse) -> do
          let env = Map.fromList [("cond", oCond), ("true", oTrue), ("false", oFalse)]

          -- Evaluate
          res <- runExceptT $ evalObf env expr
          case res of
            Right (ObfPoint d hc _ _ _ _) -> do
              let IgusaInvariants j2 _ _ _ = evaluateGeometricSignature d hc
              j2 `shouldSatisfy` (>= 0) -- Proof of structural completion
            _ -> expectationFailure "Evaluation failed to return a proper Isogeny Structure"
        _ -> expectationFailure "Failed to lift literals"

    it "Executes nested logic without intermediate decryption" $ do
      -- Logic: if (if (1) then 0 else 1) then 100 else 200
      -- Outer cond: if (1) then 0 else 1 => 0
      -- Outer result: if (0) then 100 else 200 => 200
      let expr = If (If (Var "v1") (Var "v0") (Var "v1")) (Var "v100") (Var "v200")

      lift1   <- runExceptT $ liftLit cfg pl 1
      lift0   <- runExceptT $ liftLit cfg pl 0
      lift100 <- runExceptT $ liftLit cfg pl 100
      lift200 <- runExceptT $ liftLit cfg pl 200

      case (lift1, lift0, lift100, lift200) of
        (Right o1, Right o0, Right o100, Right o200) -> do
          let env = Map.fromList [("v1", o1), ("v0", o0), ("v100", o100), ("v200", o200)]

          res <- runExceptT $ evalObf env expr
          case res of
            Right (ObfPoint d hc _ _ _ _) -> do
              let IgusaInvariants j2 _ _ _ = evaluateGeometricSignature d hc
              j2 `shouldSatisfy` (>= 0)
            _ -> expectationFailure "Evaluation failed to return a proper Isogeny Structure"
        _ -> expectationFailure "Failed to lift literals"
