-- |
-- Module      : Algebirc.Analysis.Leakage
-- Description : Algebraic leakage and linearization detection
-- License     : MIT
--
-- = Design Constraints
--
-- 1. __Cheap runtime__ — O(n) in pipeline length for most checks.
--    Entropy estimation is O(k) where k = term count (not field size).
-- 2. __No-op when disabled__ — returns clean report when analysis is off.
-- 3. __Conservative__ — flags potential weaknesses, doesn't prove security.
--
-- = Attacks Detected
--
-- 1. __Affine chain collapse__: N affines reduce to one: a_final = ∏ a_i,
--    b_final = f(a_i, b_i). Reduces security of N layers to 1 layer.
-- 2. __Polynomial linearization__: degree-1 poly sub = disguised affine.
-- 3. __Low entropy__: coefficient distribution too uniform or skewed.
-- 4. __Scheduler predictability__: priority formula is public, so ordering
--    is inherently deterministic (not a vulnerability per se).

module Algebirc.Analysis.Leakage
  ( -- * Reports
    LeakageReport(..)
  , LeakageFinding(..)
  , Severity(..)
    -- * Analysis
  , analyzeLeakage
    -- * Individual Checks
  , detectAffineCollapse
  , detectLinearization
  , estimateEntropy
  , schedulerPredictability
  , securityScore
    -- * Formatting
  , formatLeakageReport
  ) where

import Algebirc.Core.Types

-- ============================================================
-- Types
-- ============================================================

data Severity = INFO | LOW | MEDIUM | HIGH | CRITICAL
  deriving (Show, Eq, Ord)

data LeakageFinding = LeakageFinding
  { lfSeverity :: !Severity
  , lfCategory :: !String
  , lfMessage  :: !String
  } deriving (Show, Eq)

data LeakageReport = LeakageReport
  { lrFindings      :: ![LeakageFinding]
  , lrSecurityScore :: !Int              -- ^ 0-100 (100 = max security)
  , lrSummary       :: !String
  } deriving (Show, Eq)

-- ============================================================
-- Full Analysis
-- ============================================================

-- | Run all leakage checks on a pipeline.
-- O(n) in pipeline length. Cheap.
analyzeLeakage :: ObfuscationConfig
              -> [Transform]
              -> Maybe BoundedPoly    -- ^ Optional: encoded data for entropy check
              -> LeakageReport
analyzeLeakage cfg transforms maybePoly
  | not (cfgEnableAnalysis cfg) =
      LeakageReport
        { lrFindings = []
        , lrSecurityScore = -1  -- analysis disabled
        , lrSummary = "Analysis disabled (production mode)"
        }
  | otherwise =
      let collapseFindings = detectAffineCollapse cfg transforms
          linearFindings   = detectLinearization transforms
          entropyFindings  = case maybePoly of
                               Just poly -> estimateEntropy cfg poly
                               Nothing   -> []
          schedFindings    = schedulerPredictability cfg
          allFindings      = collapseFindings ++ linearFindings
                          ++ entropyFindings ++ schedFindings
          score = securityScore transforms allFindings
      in LeakageReport
           { lrFindings = allFindings
           , lrSecurityScore = score
           , lrSummary = scoreSummary score
           }

-- ============================================================
-- 1. Affine Chain Collapse
-- ============================================================

-- | Detect if consecutive affine transforms collapse to a single one.
-- N affines: x → a_n(a_{n-1}(...(a_1 x + b_1)...) + b_{n-1}) + b_n
-- Collapses to: x → (∏ a_i) x + (combined b)
-- This is O(n) — just scan the transform list.
detectAffineCollapse :: ObfuscationConfig -> [Transform] -> [LeakageFinding]
detectAffineCollapse cfg transforms =
  let p = cfgFieldPrime cfg
      runs = affineRuns transforms
      findings = concatMap (checkRun p) runs
  in findings
  where
    -- Find consecutive runs of affine transforms
    affineRuns :: [Transform] -> [(Int, [(Integer, Integer)])]
    affineRuns ts = collectRuns 0 [] ts

    collectRuns :: Int -> [(Integer, Integer)] -> [Transform] -> [(Int, [(Integer, Integer)])]
    collectRuns _   current [] =
      if length current >= 2 then [(length current, reverse current)] else []
    collectRuns idx current (t:ts) =
      case (transformTag t, transformA t, transformB t) of
        (AffineTransform, Just a, Just b) ->
          collectRuns (idx + 1) ((a, b) : current) ts
        _ ->
          let emit = if length current >= 2
                     then [(length current, reverse current)]
                     else []
          in emit ++ collectRuns (idx + 1) [] ts

    checkRun :: Integer -> (Int, [(Integer, Integer)]) -> [LeakageFinding]
    checkRun p (runLen, pairs) =
      let -- Collapse N affines to single: a_final = ∏ a_i (mod p)
          aFinal = foldl (\acc (a, _) -> (acc * a) `mod` p) 1 pairs
          -- b_final = computed recursively
          bFinal = collapseB p pairs
      in [ LeakageFinding
             { lfSeverity = if runLen >= 5 then HIGH else MEDIUM
             , lfCategory = "AFFINE_COLLAPSE"
             , lfMessage  = show runLen ++ " consecutive affine transforms collapse to single: "
                         ++ "x → " ++ show aFinal ++ "x + " ++ show bFinal
                         ++ " (mod " ++ show p ++ ")"
             } ]

-- | Compute the collapsed b value for a chain of affines.
-- For chain [(a1,b1), (a2,b2), ...]:
-- After step 1: a1*x + b1
-- After step 2: a2*(a1*x + b1) + b2 = a2*a1*x + a2*b1 + b2
-- General: b_final = a_n * b_{n-1,final} + b_n
collapseB :: Integer -> [(Integer, Integer)] -> Integer
collapseB p = foldl (\bAcc (a, b) -> (a * bAcc + b) `mod` p) 0

-- ============================================================
-- 2. Linearization Detection
-- ============================================================

-- | Check if polynomial transforms are secretly linear (degree-1).
-- A degree-1 poly sub is just a disguised affine transform.
detectLinearization :: [Transform] -> [LeakageFinding]
detectLinearization = concatMap checkOne . zip [1..]
  where
    checkOne :: (Int, Transform) -> [LeakageFinding]
    checkOne (idx, t) = case transformTag t of
      PolynomialTransform ->
        case transformPoly t of
          Just poly ->
            let deg = polyDegree poly
            in if deg <= 1
               then [ LeakageFinding
                        { lfSeverity = MEDIUM
                        , lfCategory = "LINEARIZATION"
                        , lfMessage  = "Transform #" ++ show idx
                                    ++ ": degree-" ++ show deg
                                    ++ " polynomial is effectively affine"
                                    ++ " — provides no additional security over affine"
                        } ]
               else []
          Nothing -> []
      CompositeTransform ->
        detectLinearization (transformSubs t)
      _ -> []

-- ============================================================
-- 3. Entropy Estimation
-- ============================================================

-- | Estimate Shannon entropy of coefficient distribution.
-- Higher entropy = more uniform distribution = harder to analyze.
-- O(k) where k = number of terms, NOT O(p).
--
-- We compute:
-- * Frequency of each coefficient value
-- * Shannon entropy H = -∑ p_i * log2(p_i)
-- * Compare to maximum possible entropy (log2(k))
estimateEntropy :: ObfuscationConfig -> BoundedPoly -> [LeakageFinding]
estimateEntropy cfg poly =
  let terms = polyTerms poly
      coeffs = map termCoeff terms
      n = length coeffs
  in if n == 0
     then [ LeakageFinding INFO "ENTROPY" "Empty polynomial — no entropy to measure" ]
     else
       let -- Count coefficient value frequencies
           freqs = countFreqs coeffs
           totalD = fromIntegral n :: Double
           -- Shannon entropy
           entropy = negate $ sum [ let pI = fromIntegral cnt / totalD
                                    in if pI > 0 then pI * logBase 2 pI else 0
                                  | cnt <- freqs ]
           maxEntropy = logBase 2 (fromIntegral n :: Double)
           ratio = if maxEntropy > 0 then entropy / maxEntropy else 0
           p = cfgFieldPrime cfg
           -- Also check: are all coeffs in valid range?
           allValid = all (\c -> c >= 0 && c < p) coeffs
       in
         [ LeakageFinding
             { lfSeverity = if ratio < 0.3 then HIGH
                            else if ratio < 0.6 then MEDIUM
                            else INFO
             , lfCategory = "ENTROPY"
             , lfMessage  = "Coefficient entropy: "
                         ++ showF 2 entropy ++ " / "
                         ++ showF 2 maxEntropy ++ " bits"
                         ++ " (ratio=" ++ showF 2 ratio ++ ")"
                         ++ if ratio < 0.5
                            then " — LOW entropy, distribution is predictable"
                            else " — acceptable entropy"
             } ]
         ++ [ LeakageFinding HIGH "RANGE_VIOLATION"
                ("Coefficients outside GF(" ++ show p ++ ") range detected")
            | not allValid ]

-- | Count frequencies of values. O(n log n) worst case via simple sort.
-- Returns list of counts (not the values themselves — we only need entropy).
countFreqs :: [Integer] -> [Int]
countFreqs [] = []
countFreqs xs =
  let sorted = quickSort xs
  in map length (group sorted)
  where
    quickSort [] = []
    quickSort (x:rest) = quickSort [y | y <- rest, y < x]
                      ++ [x]
                      ++ quickSort [y | y <- rest, y >= x]
    group [] = []
    group (x:rest) = let (same, diff) = span (== x) rest
                     in (x : same) : group diff

-- ============================================================
-- 4. Scheduler Predictability
-- ============================================================

-- | Analyze scheduler predictability.
-- The priority formula ∑ h_i * g^i (mod p) is deterministic by design.
-- This is not a vulnerability — it's a design choice. We flag it as INFO.
schedulerPredictability :: ObfuscationConfig -> [LeakageFinding]
schedulerPredictability _cfg =
  [ LeakageFinding
      { lfSeverity = LOW
      , lfCategory = "SCHEDULER"
      , lfMessage  = "Priority = ∑ h_i·gⁱ (mod p) with public generator g=3"
                  ++ " — ordering is deterministic given content hash"
                  ++ " (by design, not a vulnerability)"
      } ]

-- ============================================================
-- Security Score (0-100)
-- ============================================================

-- | Compute overall security score based on findings and pipeline structure.
-- Higher = more secure.
securityScore :: [Transform] -> [LeakageFinding] -> Int
securityScore transforms findings =
  let baseScore = 100
      -- Penalty per finding, scaled by severity
      penalties = sum $ map findingPenalty findings
      -- Bonus for pipeline diversity
      diversityBonus = pipelineDiversityBonus transforms
      -- Bonus for pipeline depth
      depthBonus = min 15 (length transforms * 2)
      raw = baseScore - penalties + diversityBonus + depthBonus
  in max 0 (min 100 raw)

findingPenalty :: LeakageFinding -> Int
findingPenalty f = case lfSeverity f of
  CRITICAL -> 40
  HIGH     -> 25
  MEDIUM   -> 15
  LOW      -> 5
  INFO     -> 0

-- | Bonus for using diverse transform types.
-- Using only affines = 0 bonus. Mixing affine + perm + poly = more.
pipelineDiversityBonus :: [Transform] -> Int
pipelineDiversityBonus ts =
  let tags = map transformTag ts
      hasAffine = any (== AffineTransform) tags
      hasPerm   = any (== PermutationTransform) tags
      hasPoly   = any (== PolynomialTransform) tags
      diversity = length (filter id [hasAffine, hasPerm, hasPoly])
  in case diversity of
       0 -> 0
       1 -> 0
       2 -> 10
       _ -> 20

-- ============================================================
-- Formatting
-- ============================================================

-- | Full leakage report as formatted string.
formatLeakageReport :: LeakageReport -> String
formatLeakageReport lr =
  let header = "═══ Security Analysis Report ═══\n"
      findingsStr = if null (lrFindings lr)
                    then "  No findings.\n"
                    else unlines $ map formatFinding (lrFindings lr)
      scoreStr = "\nSecurity Score: " ++ show (lrSecurityScore lr) ++ "/100"
              ++ " — " ++ lrSummary lr
  in header ++ findingsStr ++ scoreStr

formatFinding :: LeakageFinding -> String
formatFinding f =
  let sev = case lfSeverity f of
              CRITICAL -> "[!!!]"
              HIGH     -> "[!! ]"
              MEDIUM   -> "[ ! ]"
              LOW      -> "[ . ]"
              INFO     -> "[ i ]"
  in "  " ++ sev ++ " " ++ lfCategory f ++ ": " ++ lfMessage f

scoreSummary :: Int -> String
scoreSummary s
  | s >= 80   = "STRONG — pipeline provides meaningful obfuscation"
  | s >= 60   = "MODERATE — some algebraic weaknesses detected"
  | s >= 40   = "WEAK — significant algebraic leakage found"
  | otherwise = "CRITICAL — pipeline is cosmetically complex but algebraically trivial"

showF :: Int -> Double -> String
showF n x = let factor = 10 ^ n :: Int
                rounded = fromIntegral (round (x * fromIntegral factor) :: Int)
                        / fromIntegral factor :: Double
            in show rounded
