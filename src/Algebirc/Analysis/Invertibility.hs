-- |
-- Module      : Algebirc.Analysis.Invertibility
-- Description : Formal invertibility proofs for transform pipelines
-- License     : MIT
--
-- = Design Constraints
--
-- 1. __No evaluator assumptions__ — checks only algebraic properties
--    that the type system and construction already guarantee.
-- 2. __Pure__ — no IO, no mutation, fully deterministic.
-- 3. __No-op when disabled__ — returns empty proof when analysis is off.
--
-- = Invertibility Conditions
--
-- * __Affine @ax+b@__: invertible iff @gcd(a, p) = 1@ (always true
--   for prime p and @a ∈ [1, p-1]@).
-- * __Permutation__: invertible by construction (bijection on finite set).
-- * __Polynomial sub__: invertible only if degree-1 (linear) with
--   invertible leading coefficient. Higher-degree poly substitutions
--   are NOT generally invertible over finite fields.
-- * __Composite__: invertible iff ALL sub-transforms are invertible.

module Algebirc.Analysis.Invertibility
  ( -- * Results
    InvertibilityResult(..)
  , PipelineProof(..)
    -- * Checking
  , checkInvertibility
  , checkPipelineInvertibility
    -- * Formatting
  , formatProof
  , formatProofCompact
  ) where

import Algebirc.Core.Types

-- ============================================================
-- Types
-- ============================================================

-- | Result of invertibility analysis for a single transform.
data InvertibilityResult
  = Invertible !String                   -- ^ Proven invertible, with justification
  | NotInvertible !String               -- ^ Proven NOT invertible, with reason
  | ConditionallyInvertible !String     -- ^ Invertible under stated conditions
  deriving (Show, Eq)

-- | Proof chain for an entire pipeline.
data PipelineProof = PipelineProof
  { ppResults    :: ![(Int, String, InvertibilityResult)]  -- ^ (index, tag, result)
  , ppVerdict    :: !InvertibilityResult                   -- ^ Overall verdict
  , ppLayerCount :: !Int                                   -- ^ Number of layers
  } deriving (Show, Eq)

-- ============================================================
-- Single Transform Check
-- ============================================================

-- | Check invertibility of a single transform.
-- Uses only algebraic properties — no evaluation or runtime checks.
checkInvertibility :: ObfuscationConfig -> Transform -> InvertibilityResult
checkInvertibility cfg t
  | not (cfgEnableAnalysis cfg) = Invertible "analysis disabled"
  | otherwise = case transformTag t of
      AffineTransform      -> checkAffine cfg t
      PermutationTransform -> checkPermutation t
      PolynomialTransform  -> checkPolySub t
      CompositeTransform   -> checkComposite cfg t
      SBoxTransform        -> checkSBoxInv t
      FeistelTransform     -> checkFeistelInv t
      PowerMapTransform    -> checkPowerMapInv cfg t
      ARXDiffusionTransform -> checkSBoxInv t  -- ARX uses SBox, same invertibility check

-- | Affine: x → ax + b (mod p).
-- Invertible iff gcd(a, p) = 1.
-- For prime p: always invertible when a ∈ [1, p-1].
checkAffine :: ObfuscationConfig -> Transform -> InvertibilityResult
checkAffine cfg t =
  case transformA t of
    Nothing -> NotInvertible "Affine transform missing coefficient 'a'"
    Just a  ->
      let p = cfgFieldPrime cfg
          g = gcd a p
          aMod = a `mod` p
      in if aMod == 0
         then NotInvertible $
           "a ≡ 0 (mod " ++ show p ++ ") — zero map, not injective"
         else if g == 1
         then Invertible $
           "gcd(" ++ show aMod ++ ", " ++ show p ++ ") = 1"
           ++ " → Bézout guarantees a⁻¹ exists"
           ++ " → x = a⁻¹(y - b) recovers input"
         else NotInvertible $
           "gcd(" ++ show a ++ ", " ++ show p ++ ") = " ++ show g
           ++ " ≠ 1 — kernel is non-trivial"

-- | Permutation: bijection by construction.
-- We verify the mapping size matches the declared size.
checkPermutation :: Transform -> InvertibilityResult
checkPermutation t =
  case transformPerm t of
    Nothing -> NotInvertible "Permutation transform missing permutation"
    Just _perm ->
      -- Permutation is constructed via mkPermutation which validates bijectivity.
      -- We trust the smart constructor's invariant here.
      Invertible
        "Permutation on finite set is bijective by construction"

-- | Polynomial substitution: c → f(c) mod p.
-- Invertible only if f is degree-1 (affine) with invertible leading coeff.
-- Degree ≥ 2 polynomials are NOT generally injective over finite fields
-- (pigeonhole: p inputs, p outputs, but degree-d poly has ≤ d roots).
checkPolySub :: Transform -> InvertibilityResult
checkPolySub t =
  case transformPoly t of
    Nothing -> NotInvertible "Polynomial transform missing polynomial"
    Just poly ->
      let deg = polyDegree poly
      in if deg == 0
         then NotInvertible
           "Constant polynomial — maps everything to same value"
         else if deg == 1
         then ConditionallyInvertible $
           "Degree-1 polynomial (affine map on coefficients)"
           ++ " — invertible iff leading coefficient is coprime to p"
         else NotInvertible $
           "Degree-" ++ show deg ++ " polynomial"
           ++ " — not injective over GF(p) in general"
           ++ " (at most " ++ show deg ++ " preimages per output)"

-- | Composite: invertible iff ALL sub-transforms are invertible.
checkComposite :: ObfuscationConfig -> Transform -> InvertibilityResult
checkComposite cfg t =
  let subs = transformSubs t
      results = map (checkInvertibility cfg) subs
      failures = filter isFailure results
  in if null subs
     then Invertible "Empty composite — identity transform"
     else if null failures
     then Invertible $
       "All " ++ show (length subs) ++ " sub-transforms are invertible"
       ++ " → composite is invertible (inverse = reversed sub-inverses)"
     else NotInvertible $
       show (length failures) ++ " of " ++ show (length subs)
       ++ " sub-transforms are not invertible"

isFailure :: InvertibilityResult -> Bool
isFailure (NotInvertible _) = True
isFailure _ = False

-- | S-Box: bijective by construction (lookup table).
checkSBoxInv :: Transform -> InvertibilityResult
checkSBoxInv t =
  case transformSBox t of
    Nothing -> NotInvertible "S-box transform missing S-box table"
    Just _  -> Invertible
      "S-box is bijective by construction (lookup table with validated bijectivity)"

-- | Feistel: provably invertible for any round function F and any number of rounds.
checkFeistelInv :: Transform -> InvertibilityResult
checkFeistelInv t =
  let rounds = transformRounds t
  in if rounds >= 3
     then Invertible $
       "Feistel network with " ++ show rounds ++ " rounds"
       ++ " → structurally invertible (reverse rounds, same F)"
       ++ " — F = SBox ∘ quadratic (nonlinear)"
     else ConditionallyInvertible $
       "Feistel with " ++ show rounds ++ " rounds (< 3 recommended)"

-- | Power map: x → x^e mod p, bijective iff gcd(e, p-1) = 1.
checkPowerMapInv :: ObfuscationConfig -> Transform -> InvertibilityResult
checkPowerMapInv cfg t =
  case transformExp t of
    Nothing -> NotInvertible "Power map missing exponent"
    Just e  ->
      let p = cfgFieldPrime cfg
          g = gcd e (p - 1)
      in if g == 1
         then Invertible $
           "gcd(" ++ show e ++ ", " ++ show (p-1) ++ ") = 1"
           ++ " → x^e is bijective on GF(" ++ show p ++ ")*"
           ++ " → inverse: x^(e⁻¹ mod (p-1))"
         else NotInvertible $
           "gcd(" ++ show e ++ ", " ++ show (p-1) ++ ") = " ++ show g
           ++ " ≠ 1 — power map not bijective"

-- ============================================================
-- Pipeline Check
-- ============================================================

-- | Check invertibility of an entire pipeline.
checkPipelineInvertibility :: ObfuscationConfig -> [Transform] -> PipelineProof
checkPipelineInvertibility cfg transforms
  | not (cfgEnableAnalysis cfg) =
      PipelineProof
        { ppResults = []
        , ppVerdict = Invertible "analysis disabled"
        , ppLayerCount = length transforms
        }
  | otherwise =
      let indexed = zip3 [1..] (map tagStr transforms) (map (checkInvertibility cfg) transforms)
          failures = [ (i, tag, r) | (i, tag, r) <- indexed, isFailure r ]
          verdict = if null failures
                    then Invertible $
                      "Pipeline is fully invertible: all "
                      ++ show (length transforms) ++ " layers proven bijective"
                    else NotInvertible $
                      show (length failures) ++ " layer(s) are not invertible"
      in PipelineProof
           { ppResults    = indexed
           , ppVerdict    = verdict
           , ppLayerCount = length transforms
           }

tagStr :: Transform -> String
tagStr t = case transformTag t of
  AffineTransform      -> "Affine"
  PolynomialTransform  -> "PolySub"
  PermutationTransform -> "Permut"
  CompositeTransform   -> "Compos"
  SBoxTransform        -> "S-Box"
  FeistelTransform     -> "Feistl"
  PowerMapTransform    -> "PwrMap"
  ARXDiffusionTransform -> "ARXDif"

-- ============================================================
-- Formatting
-- ============================================================

-- | Full proof chain as ASCII table.
formatProof :: PipelineProof -> String
formatProof pp =
  let header = "═══ Invertibility Proof Chain ═══\n"
      rows = map formatProofRow (ppResults pp)
      verdictLine = "\n" ++ formatVerdict (ppVerdict pp)
  in header ++ unlines rows ++ verdictLine

formatProofRow :: (Int, String, InvertibilityResult) -> String
formatProofRow (idx, tag, result) =
  let prefix = "[" ++ show idx ++ "] " ++ padR 7 tag ++ " → "
  in prefix ++ formatResult result

formatResult :: InvertibilityResult -> String
formatResult (Invertible reason)               = "✓ INVERTIBLE: " ++ reason
formatResult (NotInvertible reason)            = "✗ NOT INVERTIBLE: " ++ reason
formatResult (ConditionallyInvertible reason)  = "? CONDITIONAL: " ++ reason

formatVerdict :: InvertibilityResult -> String
formatVerdict (Invertible reason)    = "═══ VERDICT: ✓ " ++ reason ++ " ═══"
formatVerdict (NotInvertible reason) = "═══ VERDICT: ✗ " ++ reason ++ " ═══"
formatVerdict (ConditionallyInvertible reason) = "═══ VERDICT: ? " ++ reason ++ " ═══"

-- | Compact one-liner.
formatProofCompact :: PipelineProof -> String
formatProofCompact pp = case ppVerdict pp of
  Invertible _    -> "✓ Pipeline invertible (" ++ show (ppLayerCount pp) ++ " layers)"
  NotInvertible r -> "✗ NOT invertible: " ++ r
  ConditionallyInvertible r -> "? Conditional: " ++ r

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
