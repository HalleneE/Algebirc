{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Algebirc.Core.Types
import Algebirc.Obfuscation.Pipeline
import Algebirc.Evaluator.Eval (Expr(..), BinOp(..))
import Algebirc.Evaluator.ObfEval
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Algebirc.Obfuscation.NonlinearTransform (powerMapPure)

main :: IO ()
main = do
  putStrLn "--- STARTING GEOMETRIC SCALAR TEST ---"
  testSBoxHomomorphism
  testLWEBinOp
  testJacobianHomomorphism

testSBoxHomomorphism :: IO ()
testSBoxHomomorphism = do
  putStrLn "\n[TEST] S-Box Homomorphism (Polynomial Mode)"
  let cfg = defaultConfig { cfgFieldPrime = 257, cfgEnableAnalysis = True, cfgMaxDegree = 256, cfgGenus = 1 }
  
  case buildPipeline cfg of
    Left err -> print err >> exitFailure
    Right pl -> do
      let valA = 42
      liftResultA <- runExceptT (liftLit cfg pl valA)
      case liftResultA of
        Right ov@(ObfLWE {}) -> do
          -- LWE Mode: decrypt-compute-reencrypt S-Box
          putStrLn "  [LWE Mode] Encrypted via RLWE symmetric-key"
          runExceptT (evalHomomorphicSBox ov) >>= \case
            Left err -> print err >> exitFailure
            Right resOV -> case lweDecrypt resOV of
              Right finalVal -> do
                let powExp = case filter (\t -> transformTag t == PowerMapTransform) (plAlgTransforms pl) of
                               (t:_) -> case transformExp t of { Just e -> e; _ -> 3 }
                               _     -> 3
                    expected = powerMapPure (cfgFieldPrime cfg) powExp valA
                putStrLn $ "Expected SBox(A): " ++ show expected
                putStrLn $ "Decrypted Result: " ++ show finalVal
                if finalVal == expected
                  then putStrLn "[PASS] S-Box Success (LWE Mode)"
                  else putStrLn "[FAIL] S-Box Failure (LWE Mode)" >> exitFailure
              Left err -> print err >> exitFailure
        Right (ObfPoly p1 c1 p2 s1) -> do
          -- Legacy Pipeline Mode
          let ovA = ObfPoly p1 c1 p2 s1
          runExceptT (evalHomomorphicSBox ovA) >>= \case
            Left err -> print err >> exitFailure
            Right resOV -> case resOV of
              ObfPoly p _ _ s -> do
                case invertPipelinePolyWithScale cfg pl p s of
                  Left err -> print err >> exitFailure
                  Right decPoly -> do
                    let finalVal = getCoeffAt 0 decPoly
                        expected = case filter (\t -> transformTag t == PowerMapTransform) (plAlgTransforms pl) of
                                     (t:_) -> case transformExp t of
                                       Just e -> powerMapPure (cfgFieldPrime cfg) e valA
                                       _ -> 0
                                     _ -> 0
                    putStrLn $ "Expected SBox(A): " ++ show expected
                    putStrLn $ "Decrypted Result: " ++ show finalVal
                    if finalVal == expected
                      then putStrLn "[PASS] S-Box Success"
                      else putStrLn "[FAIL] S-Box Failure" >> exitFailure
              _ -> putStrLn "Error: Result type mismatch" >> exitFailure
        _ -> putStrLn "Error lifting" >> exitFailure

testLWEBinOp :: IO ()
testLWEBinOp = do
  putStrLn "\n[TEST] LWE Binary Operations (Addition & Multiplication)"
  let cfg = defaultConfig { cfgFieldPrime = 257, cfgEnableAnalysis = True, cfgMaxDegree = 256, cfgGenus = 1 }
  
  case buildPipeline cfg of
    Left err -> print err >> exitFailure
    Right pl -> do
      let valA = 10
          valB = 20
      liftResultA <- runExceptT (liftLit cfg pl valA)
      liftResultB <- runExceptT (liftLit cfg pl valB)
      case (liftResultA, liftResultB) of
        (Right ovA@(ObfLWE {}), Right ovB@(ObfLWE {})) -> do
          -- Addition: [10] + [20] = [30]
          runExceptT (homomorphicBinOp Add ovA ovB) >>= \case
            Left err -> putStrLn ("Add Error: " ++ show err) >> exitFailure
            Right resAdd -> case lweDecrypt resAdd of
              Right finalAdd -> do
                putStrLn $ "  [LWE Mode] Expected Add: 30, Decrypted: " ++ show finalAdd
                if finalAdd == 30
                  then putStrLn "[PASS] LWE Addition Success (10 + 20 = 30)"
                  else putStrLn "[FAIL] LWE Addition Failure" >> exitFailure
              Left err -> putStrLn ("Add Decrypt Error: " ++ show err) >> exitFailure
          
          -- Multiplication: [10] * [20] = [200]
          runExceptT (homomorphicBinOp Mul ovA ovB) >>= \case
            Left err -> putStrLn ("Mul Error: " ++ show err) >> exitFailure
            Right resMul -> case lweDecrypt resMul of
              Right finalMul -> do
                putStrLn $ "  [LWE Mode] Expected Mul: 200, Decrypted: " ++ show finalMul
                if finalMul == 200
                  then putStrLn "[PASS] LWE Multiplication Success (10 * 20 = 200)"
                  else putStrLn "[FAIL] LWE Multiplication Failure" >> exitFailure
              Left err -> putStrLn ("Mul Decrypt Error: " ++ show err) >> exitFailure
        _ -> putStrLn "Error lifting or not LWE mode" >> exitFailure

testJacobianHomomorphism :: IO ()
testJacobianHomomorphism = do
  putStrLn "\n[TEST] Jacobian Homomorphism"
  let p = 257
      cfg = defaultConfig { cfgFieldPrime = p, cfgGenus = 2, cfgSeed = 42 }
      pl = case buildPipeline cfg of
             Right pipeline -> pipeline
             Left err -> error (show err)
      
  -- 1. Lift literals to Jacobian Points: [x]G
  let valA = 10
      valB = 20
  
  liftResA <- runExceptT (liftLit cfg pl valA)
  liftResB <- runExceptT (liftLit cfg pl valB)
  case (liftResA, liftResB) of
    (Right ovA, Right ovB) -> do
      putStrLn $ "Value A (10) lifted to Jacobian point."
      putStrLn $ "Value B (20) lifted to Jacobian point."
      
      -- 2. Perform Homomorphic Addition: [10]G + [20]G
      let env = Map.fromList [("a", ovA), ("b", ovB)]
          expr = BinOpExpr Add (Var "a") (Var "b")
      
      runExceptT (evalObf env expr) >>= \case
        Left err -> putStrLn ("Error during eval: " ++ show err) >> exitFailure
        Right resOV -> do
          -- 3. Verification: Result should be [30]G
          putStrLn "Addition complete. Decoding result via DLog..."
          
          case decodeObfPoint resOV of
            Left err -> putStrLn ("DLog Error: " ++ show err) >> exitFailure
            Right finalVal -> do
              putStrLn $ "Decoded Result: " ++ show finalVal
              if finalVal == (valA + valB)
                then putStrLn "[PASS] Jacobian Addition Success: 10 + 20 = 30 verified via DLog"
                else do
                  putStrLn $ "[FAIL] Result Mismatch! Got " ++ show finalVal ++ ", expected 30"
                  exitFailure
        _ -> putStrLn "Error lifting values" >> exitFailure
