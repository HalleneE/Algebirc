-- |
-- Module      : Algebirc.Analysis.CircuitComplexity
-- Description : Arithmetic circuit complexity as obfuscation quality proxy
-- License     : MIT
--
-- = Theory
--
-- Circuit complexity measures how many arithmetic operations (gates)
-- are needed to compute the obfuscation function. Higher gate count
-- → harder to reverse-engineer → better obfuscation.
--
-- We model the pipeline as an arithmetic circuit over GF(p):
-- * Addition gates: O(n) per affine/S-box transform
-- * Multiplication gates: O(n²) for polynomial substitution, O(n) for power map
-- * Circuit depth: sequential composition depth

module Algebirc.Analysis.CircuitComplexity
  ( -- * Analysis
    analyzeCircuit
    -- * Components
  , gateCount
  , circuitDepth
  , algebraicNormalForm
  , lowerBound
    -- * Formatting
  , formatCircuit
  ) where

import Algebirc.Core.Types

-- ============================================================
-- Main Analysis
-- ============================================================

-- | Complete circuit complexity analysis of the obfuscation pipeline.
analyzeCircuit :: ObfuscationConfig -> [Transform] -> CircuitAnalysis
analyzeCircuit cfg transforms =
  let gates = map (gateCountSingle cfg) transforms
      totalAdd  = sum (map fst gates)
      totalMul  = sum (map snd gates)
      depth     = circuitDepth transforms
      anfSize   = estimateANF cfg transforms
      lb        = lowerBound transforms
  in CircuitAnalysis
       { caGateCount  = totalAdd + totalMul
       , caMultGates  = totalMul
       , caAddGates   = totalAdd
       , caDepth      = depth
       , caANFSize    = anfSize
       , caLowerBound = lb
       }

-- ============================================================
-- Gate Counting
-- ============================================================

-- | Count arithmetic gates for a single transform.
-- Returns (addition_gates, multiplication_gates).
gateCount :: ObfuscationConfig -> Transform -> (Int, Int)
gateCount = gateCountSingle

gateCountSingle :: ObfuscationConfig -> Transform -> (Int, Int)
gateCountSingle cfg t =
  let d = cfgMaxDegree cfg
      n = d + 1  -- number of coefficient positions
  in case transformTag t of
    -- Affine: c → a*c + b for each of n positions
    -- n multiplications + n additions
    AffineTransform      -> (n, n)

    -- Permutation: just index remapping, no arithmetic gates
    -- (zero cost in circuit model)
    PermutationTransform -> (0, 0)

    -- Polynomial substitution: c → f(c) for degree-d polynomial
    -- Horner evaluation: d multiplications + d additions per coefficient
    -- Total: n * d of each
    PolynomialTransform  ->
      case transformPoly t of
        Just poly -> let deg = polyDegree poly
                     in (n * deg, n * deg)
        Nothing   -> (0, 0)

    -- Composite: sum of sub-transforms
    CompositeTransform   ->
      let subGates = map (gateCountSingle cfg) (transformSubs t)
      in (sum (map fst subGates), sum (map snd subGates))

    -- S-Box: lookup table = 0 arithmetic gates in lookup model
    -- But in circuit model, S-box requires O(p) comparisons ≈ O(p) gates
    SBoxTransform        -> (n, 0)

    -- Feistel: per round, F(x) = S(k*x² + x + k) = 1 mul + 2 add + S-box
    -- For n/2 pairs × rounds
    FeistelTransform     ->
      let rounds = transformRounds t
          pairs = n `div` 2
          addPerRound  = pairs * 3  -- quadratic eval + L' = L + F
          mulPerRound  = pairs * 1  -- k*x²
      in (addPerRound * rounds, mulPerRound * rounds)

    -- Power map: x → x^e needs O(log e) multiplications per coefficient
    -- Total: n * ceil(log₂ e)
    PowerMapTransform    ->
      case transformExp t of
        Just e  -> let logE = max 1 (ceiling (logBase 2 (fromIntegral (max 2 e) :: Double)) :: Int)
                   in (0, n * logE)
        Nothing -> (0, 0)

    -- ARX diffusion: n rounds × (3 S-box lookups + 2n additions + n muls)
    ARXDiffusionTransform ->
      let rounds = max 3 (transformRounds t)
          addPerRound = n * 3   -- forward scan + backward scan + cross-mix
          mulPerRound = n * 1   -- cross-mix nonlinear
      in (addPerRound * rounds, mulPerRound * rounds)

-- ============================================================
-- Circuit Depth
-- ============================================================

-- | Circuit depth = sum of per-layer depths.
-- Each layer contributes its own depth based on transform type.
circuitDepth :: [Transform] -> Int
circuitDepth = sum . map layerDepth
  where
    layerDepth t = case transformTag t of
      AffineTransform      -> 2    -- multiply then add
      PermutationTransform -> 0    -- zero-depth (rewiring)
      PolynomialTransform  ->
        case transformPoly t of
          Just poly -> polyDegree poly  -- Horner depth = degree
          Nothing   -> 0
      CompositeTransform   -> sum (map layerDepth (transformSubs t))
      SBoxTransform        -> 1    -- lookup (constant depth)
      FeistelTransform     -> transformRounds t * 3  -- 3 operations per round
      PowerMapTransform    ->
        case transformExp t of
          Just e  -> max 1 (ceiling (logBase 2 (fromIntegral (max 2 e) :: Double)) :: Int)
          Nothing -> 0
      ARXDiffusionTransform -> max 3 (transformRounds t) * 3  -- 3 steps per round

-- ============================================================
-- Algebraic Normal Form Size
-- ============================================================

-- | Estimate ANF (Algebraic Normal Form) size.
-- ANF represents the function as XOR of AND monomials.
-- Larger ANF → higher algebraic degree → harder to analyze.
algebraicNormalForm :: ObfuscationConfig -> [Transform] -> Int
algebraicNormalForm = estimateANF

estimateANF :: ObfuscationConfig -> [Transform] -> Int
estimateANF cfg transforms =
  let d = cfgMaxDegree cfg
      n = d + 1
      -- Each transform type contributes differently to ANF size
      anfContrib t = case transformTag t of
        AffineTransform      -> n          -- linear: n terms
        PermutationTransform -> n          -- permutation: n terms (reordered)
        PolynomialTransform  ->
          case transformPoly t of
            Just poly -> n * polyDegree poly  -- degree-d → n*d terms
            Nothing   -> n
        CompositeTransform   -> sum (map anfContrib (transformSubs t))
        SBoxTransform        -> n * n      -- S-box: high nonlinearity → O(n²) ANF terms
        FeistelTransform     ->
          let rounds = transformRounds t
          in n * rounds * 2               -- grows with rounds
        PowerMapTransform    ->
          case transformExp t of
            Just e  -> n * fromIntegral (min e 20)  -- bounded estimate
            Nothing -> n
        ARXDiffusionTransform ->
          let rounds = max 3 (transformRounds t)
          in n * n * rounds  -- high nonlinearity: O(n² × rounds)
      -- ANF size grows multiplicatively through composition
      -- but bounded by 2^n in theory
      raw = product (map (\t -> max 1 (anfContrib t)) transforms)
  in min raw (2 ^ min n 20)  -- cap at 2^n

-- ============================================================
-- Lower Bound
-- ============================================================

-- | Theoretical minimum gate count for the function class.
-- Based on the transforms used, NOT the specific parameters.
lowerBound :: [Transform] -> Int
lowerBound transforms =
  let n = length transforms
      hasNonlinear = any isNonlinearTag (map transformTag transforms)
      -- If any nonlinear transform, lower bound increases significantly
  in if hasNonlinear
     then n * 3  -- minimum 3 gates per nonlinear layer
     else n      -- linear transforms: 1 gate each minimum
  where
    isNonlinearTag SBoxTransform     = True
    isNonlinearTag FeistelTransform  = True
    isNonlinearTag PowerMapTransform = True
    isNonlinearTag ARXDiffusionTransform = True
    isNonlinearTag _                 = False

-- ============================================================
-- Formatting
-- ============================================================

-- | Format circuit analysis as ASCII report.
formatCircuit :: CircuitAnalysis -> String
formatCircuit ca = unlines
  [ "═══ Circuit Complexity Analysis ═══"
  , ""
  , "  ┌──────────────────────────────────┐"
  , "  │ Total gates     = " ++ padL 10 (show (caGateCount ca)) ++ "     │"
  , "  │   Add gates     = " ++ padL 10 (show (caAddGates ca)) ++ "     │"
  , "  │   Mul gates     = " ++ padL 10 (show (caMultGates ca)) ++ "     │"
  , "  │ Circuit depth   = " ++ padL 10 (show (caDepth ca)) ++ "     │"
  , "  │ ANF size        = " ++ padL 10 (show (caANFSize ca)) ++ "     │"
  , "  │ Lower bound     = " ++ padL 10 (show (caLowerBound ca)) ++ "     │"
  , "  └──────────────────────────────────┘"
  , ""
  , "  Overhead ratio: " ++ showRatio (caGateCount ca) (caLowerBound ca)
  , "  Mul/Add ratio:  " ++ showRatio (caMultGates ca) (caAddGates ca)
  ]

showRatio :: Int -> Int -> String
showRatio _ 0 = "∞"
showRatio a b =
  let r = fromIntegral a / fromIntegral b :: Double
      rounded = fromIntegral (round (r * 100) :: Int) / 100.0 :: Double
  in show rounded ++ "×"

padL :: Int -> String -> String
padL n s = replicate (max 0 (n - length s)) ' ' ++ s
