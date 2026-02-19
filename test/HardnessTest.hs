-- | Standalone test for Algebirc.Geometry.Hardness
-- Run: cabal run hardness-test
-- Or from GHCi: cabal repl lib:algebirc, then :load this file

module Main where

import Algebirc.Core.Types
import Algebirc.Geometry.EllipticCurve
import Algebirc.Geometry.Hardness

main :: IO ()
main = do
  putStrLn "=== Hardness Module Verification ==="
  putStrLn ""

  -- 1. Setup: non-singular curve over GF(257)
  let ec = EllipticCurve 3 7 257
  putStrLn $ "Base curve E₀:    y² = x³ + 3x + 7  (mod 257)"
  putStrLn $ "j-invariant(E₀):  " ++ show (jInvariant ec)
  putStrLn $ "Curve order:      " ++ show (curveOrder ec)
  putStrLn $ "Non-singular:     " ++ show (isNonSingular ec)
  putStrLn ""

  -- 2. Parameter validation
  let smallParams = customParams ec [3, 5, 7, 11, 13] 3
  putStrLn "--- Parameter Validation ---"
  case validateParams smallParams of
    Right _  -> putStrLn "[PASS] validateParams: params valid"
    Left err -> putStrLn $ "[FAIL] validateParams: " ++ err

  -- Negative test: singular curve
  let singularEc = EllipticCurve 0 0 257
  let badParams = customParams singularEc [3, 5] 2
  case validateParams badParams of
    Left _    -> putStrLn "[PASS] validateParams: rejects singular curve"
    Right _   -> putStrLn "[FAIL] validateParams: accepted singular curve!"

  -- Negative test: empty primes
  let emptyParams = customParams ec [] 3
  case validateParams emptyParams of
    Left _    -> putStrLn "[PASS] validateParams: rejects empty prime set"
    Right _   -> putStrLn "[FAIL] validateParams: accepted empty prime set!"
  putStrLn ""

  -- 3. Key generation
  putStrLn "--- Key Generation ---"
  let sk = generateSecretKey smallParams 42
  putStrLn $ "Secret exponents: " ++ show (skExponents sk)
  putStrLn $ "Exponents in [-3,3]: " ++
    show (all (\e -> e >= -3 && e <= 3) (skExponents sk))

  let pk = derivePublicKey sk
  putStrLn $ "Public curve Eₖ:  a=" ++ show (ecA (pkCurve pk))
                          ++ " b=" ++ show (ecB (pkCurve pk))
                          ++ " p=" ++ show (ecPrime (pkCurve pk))
  putStrLn $ "Eₖ ≠ E₀:         " ++ show (pkCurve pk /= ec)
  putStrLn $ "Same field:       " ++ show (ecPrime (pkCurve pk) == ecPrime ec)
  putStrLn ""

  -- 4. Determinism: same seed → same key
  putStrLn "--- Determinism ---"
  let sk2 = generateSecretKey smallParams 42
  let pk2 = derivePublicKey sk2
  putStrLn $ "[" ++ (if pkCurve pk == pkCurve pk2 then "PASS" else "FAIL")
           ++ "] Determinism: same seed → same public key"

  -- Different seed → different key
  let sk3 = generateSecretKey smallParams 999
  let pk3 = derivePublicKey sk3
  putStrLn $ "[" ++ (if pkCurve pk /= pkCurve pk3 then "PASS" else "FAIL")
           ++ "] Different seed → different public key"
  putStrLn ""

  -- 5. Security level estimation
  putStrLn "--- Security Level ---"
  let secLvl = securityLevel smallParams
  putStrLn $ "Keyspace bits:    " ++ show (slKeyspaceBits secLvl)
  putStrLn $ "Classical bits:   " ++ show (slClassicalBits secLvl)
  putStrLn $ "Quantum bits:     " ++ show (slQuantumBits secLvl)
  putStrLn $ "Assumption:       " ++ slAssumption secLvl
  putStrLn $ "[" ++ (if slKeyspaceBits secLvl > 0 then "PASS" else "FAIL")
           ++ "] Keyspace bits > 0"
  putStrLn $ "[" ++ (if slClassicalBits secLvl < slKeyspaceBits secLvl then "PASS" else "FAIL")
           ++ "] Classical bits < keyspace bits (MITM halving)"

  -- CSIDH-512 security
  let sec512 = securityLevel (csidh512Params ec)
  putStrLn $ "CSIDH-512 keyspace: " ++ show (slKeyspaceBits sec512) ++ " bits"
  putStrLn $ "CSIDH-512 classical: " ++ show (slClassicalBits sec512) ++ " bits"
  putStrLn ""

  -- 6. GAIP Witness
  putStrLn "--- GAIP Witness ---"
  let witness = csidh_GA_Problem sk
  putStrLn $ "Witness valid:    " ++ show (verifyWitness witness)
  putStrLn $ "[" ++ (if verifyWitness witness then "PASS" else "FAIL")
           ++ "] GAIP witness verification"
  putStrLn ""

  -- 7. Roundtrip: obfuscate → deobfuscate
  putStrLn "--- Coefficient Roundtrip ---"
  let original = [10, 20, 30, 40, 50]
  putStrLn $ "Original:         " ++ show original
  let (obfuscated, wit) = obfuscateWithCSIDH sk original
  putStrLn $ "Obfuscated:       " ++ show obfuscated
  putStrLn $ "Has GAIP witness: " ++ show (verifyWitness wit)
  let recovered = deobfuscateWithCSIDH sk obfuscated
  putStrLn $ "Recovered:        " ++ show recovered
  putStrLn $ "[" ++ (if recovered == original then "PASS" else "FAIL")
           ++ "] Roundtrip: deobfuscate(obfuscate(x)) == x"

  -- Check obfuscated ≠ original
  putStrLn $ "[" ++ (if obfuscated /= original then "PASS" else "FAIL")
           ++ "] Obfuscated ≠ original"
  putStrLn ""

  -- 8. Different keys → different obfuscation  
  putStrLn "--- Key Sensitivity ---"
  let (obf2, _) = obfuscateWithCSIDH sk3 original
  putStrLn $ "[" ++ (if obf2 /= obfuscated then "PASS" else "FAIL")
           ++ "] Different key → different obfuscation"
  putStrLn ""

  -- Summary
  putStrLn "=== Verification Complete ==="
