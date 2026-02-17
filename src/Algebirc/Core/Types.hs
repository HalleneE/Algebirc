-- |
-- Module      : Algebirc.Core.Types
-- Description : Core algebraic types with formal guarantees
-- License     : MIT
--
-- = Design Principles
--
-- 1. __Immutable Config__ — 'ObfuscationConfig' is frozen at pipeline start
--    and threaded read-only through every transform.
-- 2. __Degree Bound__ — 'maxDegree' caps polynomial composition to prevent
--    degree explosion: @deg(f . g) <= maxDegree@.
-- 3. __Canonical Form__ — polynomials have a unique representation:
--    sorted by descending exponent, no duplicate exponents, no trailing
--    zero coefficients.
-- 4. __Determinism__ — same input + same config → same output, proven by
--    construction (pure functions, no IO in transforms).

module Algebirc.Core.Types
  ( -- * Field Elements
    FieldElement(..)
  , mkFieldElement
    -- * Polynomials (Degree-Bounded)
  , BoundedPoly(..)
  , mkBoundedPoly
  , polyDegree
  , polyMaxDegree
  , polyCoefficients
  , getCoeffAt
  , Term(..)
    -- * Permutation Groups
  , Permutation(..)
  , mkPermutation
    -- * Transforms
  , Transform(..)
  , TransformTag(..)
    -- * Nonlinear Primitives
  , SBox(..)
  , mkSBox
  , sboxApply
  , sboxInvert
    -- * Algebraic Geometry Types
  , ECPoint(..)
  , EllipticCurve(..)
  , HyperCurve(..)
  , IgusaInvariants(..)
  , IsogenyPath(..)
  , MumfordDiv(..)
    -- * Secret Key & Pipeline Structure
  , SecretKey(..)
  , PipelineStructure(..)
    -- * Avalanche Metric
  , AvalancheResult(..)
    -- * Advanced Analysis Results
  , EntropyAnalysis(..)
  , AlgebraicAnalysis(..)
  , CircuitAnalysis(..)
  , AttackAnalysis(..)
    -- * Adversarial Testing Results
  , OracleAttackResult(..)
  , DiffResult(..)
  , CollapseResult(..)
  , ProbeResult(..)
  , InvariantLeak(..)
    -- * Evaluator State
  , EvalState(..)
  , emptyEvalState
    -- * Configuration (IMMUTABLE)
  , ObfuscationConfig(..)
  , defaultConfig
    -- * Typeclass
  , AlgebraicStructure(..)
    -- * Errors
  , AlgebircError(..)
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortBy, groupBy)
import Data.Ord (Down(..))
import qualified Data.Vector.Unboxed as VU

-- ============================================================
-- Field Elements: GF(p)
-- ============================================================

-- | Element of a prime finite field GF(p).
--
-- Invariant: @0 <= feValue < feModulus@
data FieldElement = FieldElement
  { feValue   :: !Integer
  , feModulus :: !Integer
  } deriving (Show, Generic, NFData)

instance Eq FieldElement where
  a == b = feValue a == feValue b && feModulus a == feModulus b

instance Ord FieldElement where
  compare a b = compare (feValue a) (feValue b)

-- | Smart constructor. Reduces value mod p.
mkFieldElement :: Integer -> Integer -> FieldElement
mkFieldElement val p
  | p <= 1    = error "FieldElement: modulus must be > 1"
  | otherwise = FieldElement (val `mod` p) p

-- ============================================================
-- Degree-Bounded Polynomials
-- ============================================================

-- | A single term: coefficient * x^exponent
data Term = Term
  { termCoeff :: !Integer
  , termExp   :: !Int
  } deriving (Show, Eq, Generic, NFData)

-- | Degree-bounded polynomial in canonical form.
--
-- __Canonical Form Invariants (Unique Representation):__
--
-- 1. Terms sorted by DESCENDING exponent
-- 2. No two terms share the same exponent
-- 3. No term has coefficient 0 (except zero polynomial = [])
-- 4. Degree <= maxDeg
--
-- These invariants guarantee:
-- * __Unique representation__: exactly one way to represent each polynomial
-- * __Order invariant__: sorted descending, always
-- * __Deterministic__: @canonicalize . canonicalize = canonicalize@
data BoundedPoly = BoundedPoly
  { polyTerms  :: ![Term]     -- ^ Terms in canonical form
  , polyMaxDeg :: !Int         -- ^ Hard degree cap
  , polyField  :: !Integer     -- ^ Coefficient modulus (0 = unbounded integers)
  } deriving (Show, Generic, NFData)

instance Eq BoundedPoly where
  a == b = polyTerms a == polyTerms b
        && polyMaxDeg a == polyMaxDeg b
        && polyField a  == polyField b

-- | Enforce canonical form on a list of terms.
canonicalize :: Integer -> Int -> [Term] -> [Term]
canonicalize fieldMod maxDeg terms =
  let -- 1. Reduce coefficients mod field (if fieldMod > 0)
      reduced = if fieldMod > 0
                then map (\(Term c e) -> Term (c `mod` fieldMod) e) terms
                else terms
      -- 2. Group by exponent, sum coefficients
      sorted  = sortBy (\a b -> compare (Down (termExp a)) (Down (termExp b))) reduced
      grouped = groupBy (\a b -> termExp a == termExp b) sorted
      merged  = map (\grp -> Term (sum (map termCoeff grp)) (termExp (head grp))) grouped
      -- 3. Remove zero coefficients
      nonZero = filter (\(Term c _) -> c /= 0) merged
      -- 4. Reduce again after summing
      reduced2 = if fieldMod > 0
                 then filter (\(Term c _) -> c `mod` fieldMod /= 0)
                           $ map (\(Term c e) -> Term (c `mod` fieldMod) e) nonZero
                 else nonZero
      -- 5. Truncate to degree cap
      capped  = filter (\(Term _ e) -> e <= maxDeg) reduced2
  in capped

-- | Smart constructor. Enforces canonical form.
mkBoundedPoly :: Integer -> Int -> [Term] -> BoundedPoly
mkBoundedPoly fieldMod maxDeg terms =
  BoundedPoly (canonicalize fieldMod maxDeg terms) maxDeg fieldMod

-- | Degree of the polynomial (0 for zero polynomial).
polyDegree :: BoundedPoly -> Int
polyDegree (BoundedPoly []    _ _) = 0
polyDegree (BoundedPoly (t:_) _ _) = termExp t  -- canonical form: first term is highest

-- | Get the max degree cap.
polyMaxDegree :: BoundedPoly -> Int
polyMaxDegree (BoundedPoly _ md _) = md

-- | Get coefficient at a specific exponent (0 if not present).
getCoeffAt :: Int -> BoundedPoly -> Integer
getCoeffAt i (BoundedPoly terms _ _) =
  case filter (\(Term _ e) -> e == i) terms of
    (Term c _):_ -> c
    []           -> 0

-- | Get all (exponent, coefficient) pairs.
polyCoefficients :: BoundedPoly -> [(Int, Integer)]
polyCoefficients (BoundedPoly terms _ _) = map (\(Term c e) -> (e, c)) terms

-- ============================================================
-- Permutation Groups
-- ============================================================

-- | Permutation on {0, 1, ..., n-1}.
--
-- Stored as a mapping i -> sigma(i).
-- Invariant: bijective (every element appears exactly once).
data Permutation = Permutation
  { permSize    :: !Int
  , permMapping :: !(Map Int Int)
  } deriving (Show, Eq, Generic, NFData)

-- | Smart constructor. Validates bijectivity.
mkPermutation :: Int -> [(Int, Int)] -> Either AlgebircError Permutation
mkPermutation n mapping
  | length mapping /= n = Left (PermutationError "Mapping size /= n")
  | any (\(i,_) -> i < 0 || i >= n) mapping = Left (PermutationError "Domain out of range")
  | any (\(_,j) -> j < 0 || j >= n) mapping = Left (PermutationError "Codomain out of range")
  | length (Map.fromList mapping) /= n = Left (PermutationError "Duplicate in domain")
  | Map.size (Map.fromList (map (\(a,b) -> (b,a)) mapping)) /= n =
      Left (PermutationError "Not bijective")
  | otherwise = Right (Permutation n (Map.fromList mapping))

-- ============================================================
-- Transforms
-- ============================================================

-- | Tag identifying the type of algebraic transform.
data TransformTag
  = AffineTransform        -- ^ x → ax + b (mod p)
  | PolynomialTransform    -- ^ evaluate polynomial
  | PermutationTransform   -- ^ byte permutation
  | CompositeTransform     -- ^ composition of transforms
  | SBoxTransform          -- ^ lookup-table bijection GF(p) → GF(p)
  | FeistelTransform       -- ^ Feistel network round (F = SBox ∘ quadratic)
  | PowerMapTransform      -- ^ x → x^e mod p, gcd(e, p-1)=1
  | ARXDiffusionTransform  -- ^ Full-width ARX mixing (Add-Rotate-XOR-like)
  -- Phase 8: Algebraic Geometry
  | IsogenyTransform       -- ^ Elliptic curve isogeny walk (Vélu)
  | RichelotTransform      -- ^ (2,2)-isogeny on genus-2 Jacobian
  | CMActionTransform      -- ^ CM group action on j-invariant
  | SiegelTransform        -- ^ Siegel modular polynomial evaluation
  deriving (Show, Eq, Generic, NFData)

-- | An algebraic transformation with metadata.
data Transform = Transform
  { transformTag    :: !TransformTag
  , transformPoly   :: !(Maybe BoundedPoly)   -- ^ polynomial (if applicable)
  , transformPerm   :: !(Maybe Permutation)    -- ^ permutation (if applicable)
  , transformA      :: !(Maybe Integer)        -- ^ 'a' coefficient for affine
  , transformB      :: !(Maybe Integer)        -- ^ 'b' coefficient for affine
  , transformSubs   :: ![Transform]            -- ^ sub-transforms for composite
  , transformSBox   :: !(Maybe SBox)           -- ^ S-box bijection table
  , transformExp    :: !(Maybe Integer)        -- ^ Power map exponent e
  , transformRounds :: !Int                    -- ^ Feistel rounds (≥3 for security)
  , transformKey    :: !(Maybe SecretKey)       -- ^ Secret key (never exposed)
  -- Phase 8: Algebraic Geometry fields
  , transformCurve  :: !(Maybe EllipticCurve)  -- ^ EC for isogeny transforms
  , transformHyper  :: !(Maybe HyperCurve)     -- ^ Genus-2 curve
  , transformIgusa  :: !(Maybe IgusaInvariants) -- ^ Igusa invariants
  , transformIsogeny :: !(Maybe IsogenyPath)    -- ^ Isogeny path data
  } deriving (Show, Eq, Generic, NFData)

-- ============================================================
-- S-Box: Nonlinear Bijection Table
-- ============================================================

-- | S-Box: a bijective lookup table GF(p) → GF(p).
-- Forward table: sboxFwd[x] = y
-- Inverse table: sboxInv[y] = x
-- Bijectivity guaranteed at construction.
data SBox = SBox
  { sboxFwd   :: !(VU.Vector Int)   -- ^ Forward mapping [0..p-1] → [0..p-1]
  , sboxInv   :: !(VU.Vector Int)   -- ^ Inverse mapping
  , sboxPrime :: !Integer           -- ^ Field prime
  } deriving (Show, Eq, Generic, NFData)

-- | Construct an S-box from a forward mapping.
-- Validates bijectivity: every element appears exactly once.
mkSBox :: Integer -> VU.Vector Int -> Either AlgebircError SBox
mkSBox p fwd
  | VU.length fwd /= fromIntegral p =
      Left (GenericError $ "S-box size " ++ show (VU.length fwd)
                        ++ " ≠ p=" ++ show p)
  | not (isBijective fwd) =
      Left (GenericError "S-box is not bijective")
  | otherwise =
      let inv = VU.replicate (fromIntegral p) 0
                  VU.// [(fwd VU.! i, i) | i <- [0 .. fromIntegral p - 1]]
      in Right $ SBox fwd inv p
  where
    isBijective v =
      let n = VU.length v
          seen = VU.replicate n False
                   VU.// [(v VU.! i, True) | i <- [0..n-1]]
      in VU.all id seen

-- | Apply S-box forward: x → S(x)
sboxApply :: SBox -> Integer -> Integer
sboxApply sb x = fromIntegral $ sboxFwd sb VU.! fromIntegral (x `mod` sboxPrime sb)

-- | Apply S-box inverse: y → S⁻¹(y)
sboxInvert :: SBox -> Integer -> Integer
sboxInvert sb y = fromIntegral $ sboxInv sb VU.! fromIntegral (y `mod` sboxPrime sb)

-- ============================================================
-- Algebraic Geometry Types (Phase 8)
-- ============================================================

-- | Point on an elliptic curve E: y² = x³ + ax + b over GF(p).
data ECPoint
  = Infinity                               -- ^ Point at infinity (identity)
  | ECPoint { ecX :: !Integer, ecY :: !Integer }  -- ^ Affine point (x, y)
  deriving (Show, Eq, Generic, NFData)

-- | Elliptic curve in short Weierstrass form: y² = x³ + ax + b (mod p).
--
-- Invariant: 4a³ + 27b² ≠ 0 (mod p) — non-singular.
data EllipticCurve = EllipticCurve
  { ecA     :: !Integer   -- ^ Coefficient a
  , ecB     :: !Integer   -- ^ Coefficient b
  , ecPrime :: !Integer   -- ^ Field prime p
  } deriving (Show, Eq, Generic, NFData)

-- | Hyperelliptic curve of genus g: y² = f(x) over GF(p).
--
-- For genus 2: deg(f) ∈ {5, 6}.
-- Coefficients stored as [f₀, f₁, ..., f_{2g+1}] or [f₀, ..., f_{2g+2}].
data HyperCurve = HyperCurve
  { hcCoeffs :: ![Integer]  -- ^ Polynomial f(x) coefficients (ascending degree)
  , hcGenus  :: !Int         -- ^ Genus (typically 2)
  , hcPrime  :: !Integer     -- ^ Field prime p
  } deriving (Show, Eq, Generic, NFData)

-- | Mumford representation of a divisor on Jac(C) for genus-2.
--
-- A semi-reduced divisor D = (u(x), v(x)) where:
--   • u(x) is monic, deg(u) ≤ g
--   • deg(v) < deg(u)
--   • u | (v² - f)
data MumfordDiv = MumfordDiv
  { mdU :: ![Integer]   -- ^ u(x) coefficients (ascending, monic)
  , mdV :: ![Integer]   -- ^ v(x) coefficients (ascending)
  , mdP :: !Integer      -- ^ Field prime
  } deriving (Show, Eq, Generic, NFData)

-- | Igusa invariants (J₂, J₄, J₆, J₁₀) classifying genus-2 curves
-- up to isomorphism over an algebraically closed field.
data IgusaInvariants = IgusaInvariants
  { igJ2  :: !Integer   -- ^ Weight-2 invariant
  , igJ4  :: !Integer   -- ^ Weight-4 invariant
  , igJ6  :: !Integer   -- ^ Weight-6 invariant
  , igJ10 :: !Integer   -- ^ Weight-10 invariant (discriminant-like)
  } deriving (Show, Eq, Generic, NFData)

-- | Isogeny path: sequence of (kernel_x, degree) describing
-- a walk in the isogeny graph E₀ →ℓ₁ E₁ →ℓ₂ E₂ → ... → Eₖ.
data IsogenyPath = IsogenyPath
  { ipSteps  :: ![(Integer, Integer)]  -- ^ [(kernel_x, ℓ)] per step
  , ipDegree :: !Int                   -- ^ Total degree = ∏ ℓᵢ
  } deriving (Show, Eq, Generic, NFData)

-- ============================================================
-- Secret Key / Pipeline Structure
-- ============================================================

-- | Secret key — NEVER exposed in public output.
-- Pipeline structure is public; this is the secret.
data SecretKey = SecretKey
  { skSeed     :: !Integer    -- ^ Master seed for deterministic generation
  , skRounds   :: !Int        -- ^ Feistel rounds (≥3)
  , skSBoxSeed :: !Integer    -- ^ S-box permutation seed
  , skPowerExp :: !Integer    -- ^ Power map exponent
  } deriving (Show, Eq, Generic, NFData)

-- | Public pipeline structure — safe to expose to attacker.
-- Security comes from SecretKey, not structure obscurity.
data PipelineStructure = PipelineStructure
  { psTransformTags :: ![TransformTag]  -- ^ Transform type sequence
  , psDegree        :: !Int             -- ^ Degree cap
  , psFieldPrime    :: !Integer         -- ^ Field modulus
  , psLayerCount    :: !Int             -- ^ Number of layers
  } deriving (Show, Eq, Generic, NFData)

-- ============================================================
-- Avalanche Metric
-- ============================================================

-- | Measures differential sensitivity:
-- Change 1 input coefficient → how many output coefficients change?
data AvalancheResult = AvalancheResult
  { arChangedCoeffs  :: !Int      -- ^ Coefficients that changed
  , arTotalCoeffs    :: !Int      -- ^ Total coefficients
  , arAvalancheRatio :: !Double   -- ^ changed/total (target: ≥ 0.5)
  , arMaxDelta       :: !Integer  -- ^ Largest coefficient difference
  , arPasses         :: !Bool     -- ^ True if ratio ≥ 0.5
  } deriving (Show, Eq, Generic, NFData)

-- ============================================================
-- Advanced Analysis Results
-- ============================================================

-- | Formal entropy analysis result.
data EntropyAnalysis = EntropyAnalysis
  { eaShannon      :: !Double     -- ^ Shannon entropy H(X)
  , eaMinEntropy   :: !Double     -- ^ Min-entropy H∞(X) = -log₂(max pᵢ)
  , eaMaxEntropy   :: !Double     -- ^ Maximum possible: log₂(|support|)
  , eaRedundancy   :: !Double     -- ^ 1 - H/H_max
  , eaAdversarial  :: !Double     -- ^ Mutual info I(in;out | structure)
  , eaGrade        :: !Char       -- ^ 'A' to 'F'
  } deriving (Show, Eq, Generic, NFData)

-- | Algebraic leakage analysis result.
data AlgebraicAnalysis = AlgebraicAnalysis
  { aaIdealDim        :: !Int       -- ^ Gröbner basis size
  , aaInvariants      :: ![String]  -- ^ Detected structural invariants
  , aaMonomialLeak    :: !Bool      -- ^ Monomial degree pattern preserved?
  , aaLinearRelations :: !Int       -- ^ # linear relations found
  } deriving (Show, Eq, Generic, NFData)

-- | Arithmetic circuit complexity result.
data CircuitAnalysis = CircuitAnalysis
  { caGateCount   :: !Int       -- ^ Total arithmetic gates
  , caMultGates   :: !Int       -- ^ Multiplication gates
  , caAddGates    :: !Int       -- ^ Addition gates
  , caDepth       :: !Int       -- ^ Circuit depth
  , caANFSize     :: !Int       -- ^ Algebraic Normal Form term count
  , caLowerBound  :: !Int       -- ^ Theoretical minimum gates
  } deriving (Show, Eq, Generic, NFData)

-- | Linearization attack modeling result.
data AttackAnalysis = AttackAnalysis
  { laMatrixRank     :: !Int       -- ^ Rank of linearization matrix
  , laKnownPairs     :: !Int       -- ^ # known pairs needed to break
  , laComplexity     :: !String    -- ^ Attack complexity
  , laVulnerable     :: !Bool      -- ^ True = pipeline vulnerable
  , laRecommendation :: !String    -- ^ Fix suggestion
  } deriving (Show, Eq, Generic, NFData)

-- ============================================================
-- Adversarial Testing Results
-- ============================================================

-- | Known/chosen-plaintext attack result.
data OracleAttackResult = OracleAttackResult
  { oaRecovered      :: !Bool      -- ^ Berhasil recover transform?
  , oaPairsUsed      :: !Int       -- ^ Pasangan input/output yang dipakai
  , oaRankDrop       :: !Double    -- ^ Matrix rank drop ratio (0 = full rank, 1 = zero)
  , oaSolutionSpace  :: !Integer   -- ^ Estimated solution space size
  , oaCorrelation    :: !Double    -- ^ Max korelasi input↔output
  , oaResidualError  :: !Double    -- ^ Error approximation (0 = exact recovery)
  , oaVulnerability  :: !String    -- ^ Deskripsi kelemahan
  } deriving (Show, Eq, Generic, NFData)

-- | Differential attack result.
data DiffResult = DiffResult
  { drDeadPositions    :: !Int     -- ^ Posisi yang tidak pengaruh output
  , drLinearPositions  :: !Int     -- ^ Posisi dengan delta proporsional
  , drAvalancheRatio   :: !Double  -- ^ Rata-rata % output yang berubah
  , drMaxCorrelation   :: !Double  -- ^ Korelasi tertinggi antar posisi
  , drIsDifferential   :: !Bool    -- ^ Ada differential characteristic?
  , drDetails          :: ![String] -- ^ Detail per-posisi
  } deriving (Show, Eq, Generic, NFData)

-- | Composition collapse result per scale.
data CollapseResult = CollapseResult
  { crLayers         :: !Int       -- ^ Jumlah layer
  , crEntropyBefore  :: !Double    -- ^ H(input)
  , crEntropyAfter   :: !Double    -- ^ H(output)
  , crEntropyDelta   :: !Double    -- ^ ΔH (negatif = collapse!)
  , crDiversity      :: !Double    -- ^ Koefisien unik / total
  , crFixedPoints    :: !Int       -- ^ Posisi yang tidak berubah
  , crJacobianRank   :: !Int       -- ^ Jacobian matrix rank
  , crExpectedRank   :: !Int       -- ^ Expected full rank
  , crDegreeGrowth   :: !Double    -- ^ actual / expected degree ratio
  , crCollapsed      :: !Bool      -- ^ True = GAGAL
  } deriving (Show, Eq, Generic, NFData)

-- | Structural probe result.
data ProbeResult = ProbeResult
  { prEffectiveDegree  :: !Int       -- ^ Degree efektif pipeline
  , prExpectedDegree   :: !Int       -- ^ Degree yang diharapkan
  , prDegreeRatio      :: !Double    -- ^ effective / expected
  , prShortcutFound    :: !Bool      -- ^ True = ADA shortcut!
  , prAnnihilatorDeg   :: !Int       -- ^ Degree annihilator terkecil (0 = none)
  , prFactorCount      :: !Int       -- ^ Jumlah faktor ditemukan
  } deriving (Show, Eq, Generic, NFData)

-- | Invariant that survived the pipeline.
data InvariantLeak = InvariantLeak
  { ilProperty :: !String    -- ^ Nama invariant
  , ilSurvives :: !Bool      -- ^ Masih ada setelah pipeline?
  , ilSeverity :: !String    -- ^ "CRITICAL" / "WARNING" / "INFO"
  } deriving (Show, Eq, Generic, NFData)

-- ============================================================
-- Evaluator State (Turing-Complete)
-- ============================================================

-- | Mutable evaluator state, threaded through StateT.
data EvalState = EvalState
  { esEnv       :: !(Map String Integer)  -- ^ Variable bindings (unbounded memory)
  , esDepth     :: !Int                   -- ^ Current recursion depth
  , esMaxDepth  :: !Int                   -- ^ Hard recursion limit (termination guarantee)
  , esStepCount :: !Int                   -- ^ Total steps executed
  , esMaxSteps  :: !Int                   -- ^ Step budget (termination guarantee)
  } deriving (Show, Eq, Generic, NFData)

emptyEvalState :: Int -> Int -> EvalState
emptyEvalState maxDepth maxSteps = EvalState
  { esEnv       = Map.empty
  , esDepth     = 0
  , esMaxDepth  = maxDepth
  , esStepCount = 0
  , esMaxSteps  = maxSteps
  }

-- ============================================================
-- Configuration (IMMUTABLE throughout pipeline)
-- ============================================================

-- | Obfuscation configuration. This record is FROZEN at pipeline
-- initialization and NEVER modified during execution.
--
-- __Reproducibility guarantee__: same 'ObfuscationConfig' + same input
-- = same output, always.
data ObfuscationConfig = ObfuscationConfig
  { cfgFieldPrime      :: !Integer   -- ^ Prime modulus for GF(p)
  , cfgMaxDegree       :: !Int       -- ^ Hard degree cap for polynomials
  , cfgMaxDepth        :: !Int       -- ^ Max recursion depth for evaluator
  , cfgMaxSteps        :: !Int       -- ^ Max eval steps (termination budget)
  , cfgSeed            :: !Integer   -- ^ Deterministic seed for transforms
  , cfgEnableAnalysis  :: !Bool      -- ^ Enable analysis modules (False in production)
  } deriving (Show, Eq, Generic, NFData)

-- | Sensible defaults.
defaultConfig :: ObfuscationConfig
defaultConfig = ObfuscationConfig
  { cfgFieldPrime     = 257          -- smallest prime > 256 (byte range)
  , cfgMaxDegree      = 64           -- degree cap
  , cfgMaxDepth       = 100          -- recursion depth limit
  , cfgMaxSteps       = 10000        -- step budget
  , cfgSeed           = 42           -- default seed
  , cfgEnableAnalysis = True         -- analysis ON (set False for production)
  }

-- ============================================================
-- Typeclass: AlgebraicStructure
-- ============================================================

-- | Typeclass for algebraic structures with identity, composition,
-- and inverse (when it exists).
class AlgebraicStructure a where
  -- | Identity element.
  identity :: a
  -- | Compose two structures.
  compose  :: a -> a -> Either AlgebircError a
  -- | Inverse (may not exist).
  inverse  :: a -> Either AlgebircError a

-- ============================================================
-- Errors
-- ============================================================

data AlgebircError
  = DegreeOverflow !Int !Int           -- ^ actual, max
  | FieldMismatch !Integer !Integer    -- ^ got, expected
  | PermutationError !String
  | EvalDepthExceeded !Int
  | EvalStepsExceeded !Int
  | DivisionByZero
  | InverseNotFound !String
  | GenericError !String
  deriving (Show, Eq, Generic, NFData)
