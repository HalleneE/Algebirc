-- |
-- Module      : Algebirc.Analysis.DegreeTracker
-- Description : Pure observational tracking of polynomial complexity per transform layer
-- License     : MIT
--
-- = Design Constraints
--
-- 1. __Pure observation__ — does NOT affect evaluation results.
--    'trackedPipeline' returns the SAME polynomial as 'applyPipeline',
--    plus a 'TrackingReport' on the side.
-- 2. __No-op when disabled__ — if 'cfgEnableAnalysis' is False,
--    falls back to plain 'applyPipeline' with an empty report.
-- 3. __Cheap__ — only counts terms and reads degree, no extra allocations.

module Algebirc.Analysis.DegreeTracker
  ( -- * Reports
    LayerSnapshot(..)
  , TrackingReport(..)
    -- * Tracked Pipeline
  , trackedPipeline
  , trackedInvertPipeline
    -- * Formatting
  , formatReport
  , formatCompact
    -- * Queries
  , maxTermCount
  , maxDensity
  , hasExplosion
  ) where

import Algebirc.Core.Types
import Algebirc.Obfuscation.Transform (applyTransform, invertTransform)

-- ============================================================
-- Types
-- ============================================================

-- | Snapshot of polynomial metrics at one pipeline layer.
-- Pure data — no references, no IO.
data LayerSnapshot = LayerSnapshot
  { lsLayer      :: !Int       -- ^ Layer index (0 = input)
  , lsTag        :: !String    -- ^ Transform tag ("Affine", "Perm", etc.)
  , lsTermCount  :: !Int       -- ^ Number of non-zero terms
  , lsDegree     :: !Int       -- ^ Current degree (highest exponent)
  , lsDegreeCap  :: !Int       -- ^ Max degree cap
  , lsDensity    :: !Double    -- ^ Density: termCount / (degree + 1)
  , lsDeltaTerms :: !Int       -- ^ Change in term count from previous layer
  } deriving (Show, Eq)

-- | Complete tracking report for a pipeline run.
data TrackingReport = TrackingReport
  { trSnapshots    :: ![LayerSnapshot]  -- ^ Per-layer snapshots
  , trDirection    :: !String           -- ^ "forward" or "inverse"
  , trLayerCount   :: !Int              -- ^ Number of transform layers
  , trInputTerms   :: !Int              -- ^ Term count of input polynomial
  , trOutputTerms  :: !Int              -- ^ Term count of output polynomial
  } deriving (Show, Eq)

-- | Empty report (returned when analysis is disabled).
emptyReport :: String -> TrackingReport
emptyReport dir = TrackingReport
  { trSnapshots  = []
  , trDirection  = dir
  , trLayerCount = 0
  , trInputTerms = 0
  , trOutputTerms = 0
  }

-- ============================================================
-- Snapshot Construction
-- ============================================================

-- | Take a snapshot of the current polynomial state.
snapshot :: Int -> String -> Int -> BoundedPoly -> LayerSnapshot
snapshot idx tag prevTerms poly =
  let tc  = length (polyTerms poly)
      deg = polyDegree poly
      cap = polyMaxDegree poly
      den = if deg > 0
            then fromIntegral tc / fromIntegral (deg + 1) :: Double
            else if tc > 0 then 1.0 else 0.0
  in LayerSnapshot
       { lsLayer      = idx
       , lsTag        = tag
       , lsTermCount  = tc
       , lsDegree     = deg
       , lsDegreeCap  = cap
       , lsDensity    = den
       , lsDeltaTerms = tc - prevTerms
       }

-- | Get a human-readable tag for a transform.
transformTagStr :: Transform -> String
transformTagStr t = case transformTag t of
  AffineTransform      -> "Affine"
  PolynomialTransform  -> "PolySub"
  PermutationTransform -> "Permut"
  CompositeTransform   -> "Compos"
  SBoxTransform        -> "S-Box"
  FeistelTransform     -> "Feistl"
  PowerMapTransform    -> "PwrMap"
  ARXDiffusionTransform -> "ARXDif"

-- ============================================================
-- Tracked Pipeline (Pure — same result as applyPipeline)
-- ============================================================

-- | Apply transforms with tracking. Returns SAME polynomial as
-- 'applyPipeline' — the report is a pure side-channel observation.
--
-- When 'cfgEnableAnalysis' is False, returns empty report.
trackedPipeline :: ObfuscationConfig
               -> [Transform]
               -> BoundedPoly
               -> Either AlgebircError (BoundedPoly, TrackingReport)
trackedPipeline cfg transforms poly
  | not (cfgEnableAnalysis cfg) =
      -- Production mode: skip tracking, just run pipeline
      case runPipeline cfg transforms poly of
        Left err -> Left err
        Right result -> Right (result, emptyReport "forward")
  | otherwise =
      let inputSnap = snapshot 0 "INPUT" 0 poly
      in case go cfg 1 (lsTermCount inputSnap) [inputSnap] transforms poly of
           Left err -> Left err
           Right (result, snaps) ->
             let allSnaps = reverse snaps
             in Right (result, TrackingReport
                  { trSnapshots  = allSnaps
                  , trDirection  = "forward"
                  , trLayerCount = length transforms
                  , trInputTerms = lsTermCount inputSnap
                  , trOutputTerms = length (polyTerms result)
                  })
  where
    go _ _ _ snaps [] p = Right (p, snaps)
    go c idx prevTC snaps (t:ts) p =
      case applyTransform c t p of
        Left err -> Left err
        Right result ->
          let snap = snapshot idx (transformTagStr t) prevTC result
          in go c (idx + 1) (lsTermCount snap) (snap : snaps) ts result

-- | Invert transforms with tracking.
trackedInvertPipeline :: ObfuscationConfig
                     -> [Transform]
                     -> BoundedPoly
                     -> Either AlgebircError (BoundedPoly, TrackingReport)
trackedInvertPipeline cfg transforms poly
  | not (cfgEnableAnalysis cfg) =
      case runInvertPipeline cfg transforms poly of
        Left err -> Left err
        Right result -> Right (result, emptyReport "inverse")
  | otherwise =
      let inputSnap = snapshot 0 "INPUT" 0 poly
          reversed = reverse transforms
      in case go cfg 1 (lsTermCount inputSnap) [inputSnap] reversed poly of
           Left err -> Left err
           Right (result, snaps) ->
             let allSnaps = reverse snaps
             in Right (result, TrackingReport
                  { trSnapshots  = allSnaps
                  , trDirection  = "inverse"
                  , trLayerCount = length transforms
                  , trInputTerms = lsTermCount inputSnap
                  , trOutputTerms = length (polyTerms result)
                  })
  where
    go _ _ _ snaps [] p = Right (p, snaps)
    go c idx prevTC snaps (t:ts) p =
      case invertTransform c t p of
        Left err -> Left err
        Right result ->
          let snap = snapshot idx (transformTagStr t ++ "⁻¹") prevTC result
          in go c (idx + 1) (lsTermCount snap) (snap : snaps) ts result

-- | Plain pipeline (no tracking). Used internally as fallback.
runPipeline :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
runPipeline _ [] p = Right p
runPipeline c (t:ts) p = applyTransform c t p >>= runPipeline c ts

-- | Plain invert pipeline.
runInvertPipeline :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
runInvertPipeline _ [] p = Right p
runInvertPipeline c ts p =
  let reversed = reverse ts
  in foldlE (\acc t -> invertTransform c t acc) p reversed

foldlE :: (a -> b -> Either e a) -> a -> [b] -> Either e a
foldlE _ acc [] = Right acc
foldlE f acc (x:xs) = f acc x >>= \acc' -> foldlE f acc' xs

-- ============================================================
-- Formatting (ASCII Table)
-- ============================================================

-- | Full ASCII table visualization.
--
-- @
-- ┌───────┬────────┬───────┬─────────┬────────┬──────────┐
-- │ Layer │  Tag   │ Terms │ Density │ Degree │ Δ Terms  │
-- ├───────┼────────┼───────┼─────────┼────────┼──────────┤
-- │     0 │ INPUT  │     3 │   4.62% │      2 │        — │
-- │     1 │ Affine │    65 │ 100.0%  │     64 │      +62 │
-- │     2 │ Permut │    65 │ 100.0%  │     64 │        0 │
-- └───────┴────────┴───────┴─────────┴────────┴──────────┘
-- @
formatReport :: TrackingReport -> String
formatReport tr =
  let snaps = trSnapshots tr
      header = "┌───────┬────────┬───────┬─────────┬────────┬──────────┐\n"
            ++ "│ Layer │  Tag   │ Terms │ Density │ Degree │ Δ Terms  │\n"
            ++ "├───────┼────────┼───────┼─────────┼────────┼──────────┤\n"
      footer = "└───────┴────────┴───────┴─────────┴────────┴──────────┘"
      rows = map formatRow snaps
      summary = "\n[" ++ trDirection tr ++ "] "
             ++ show (trLayerCount tr) ++ " layers, "
             ++ show (trInputTerms tr) ++ " → "
             ++ show (trOutputTerms tr) ++ " terms"
  in header ++ unlines rows ++ footer ++ summary

formatRow :: LayerSnapshot -> String
formatRow ls =
  let layerStr = padL 5 (show (lsLayer ls))
      tagStr   = padR 6 (take 6 (lsTag ls))
      termStr  = padL 5 (show (lsTermCount ls))
      denStr   = padL 7 (showDensity (lsDensity ls))
      degStr   = padL 6 (show (lsDegree ls))
      deltaStr = padL 8 (showDelta ls)
  in "│ " ++ layerStr ++ " │ " ++ tagStr ++ " │ " ++ termStr
     ++ " │ " ++ denStr ++ " │ " ++ degStr ++ " │ " ++ deltaStr ++ " │"

-- | Compact one-line-per-layer format.
formatCompact :: TrackingReport -> String
formatCompact tr = unlines $ map compactLine (trSnapshots tr)
  where
    compactLine ls = "[" ++ show (lsLayer ls) ++ "] "
                  ++ lsTag ls ++ ": "
                  ++ show (lsTermCount ls) ++ " terms, "
                  ++ "deg=" ++ show (lsDegree ls) ++ ", "
                  ++ "density=" ++ showDensity (lsDensity ls)
                  ++ case lsDeltaTerms ls of
                       0 -> ""
                       d -> " (Δ" ++ showSigned d ++ ")"

showDensity :: Double -> String
showDensity d = let pct = d * 100
                    rounded = fromIntegral (round (pct * 10) :: Int) / 10.0 :: Double
                in show rounded ++ "%"

showDelta :: LayerSnapshot -> String
showDelta ls
  | lsLayer ls == 0 = "—"
  | lsDeltaTerms ls == 0 = "0"
  | lsDeltaTerms ls > 0 = "+" ++ show (lsDeltaTerms ls)
  | otherwise = show (lsDeltaTerms ls)

showSigned :: Int -> String
showSigned n
  | n > 0     = "+" ++ show n
  | otherwise = show n

padL :: Int -> String -> String
padL n s = replicate (max 0 (n - length s)) ' ' ++ s

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '

-- ============================================================
-- Queries
-- ============================================================

-- | Maximum term count observed across all layers.
maxTermCount :: TrackingReport -> Int
maxTermCount tr = case trSnapshots tr of
  [] -> 0
  ss -> maximum (map lsTermCount ss)

-- | Maximum density observed (0.0 to 1.0).
maxDensity :: TrackingReport -> Double
maxDensity tr = case trSnapshots tr of
  [] -> 0.0
  ss -> maximum (map lsDensity ss)

-- | Did term count ever exceed a given threshold?
hasExplosion :: Int -> TrackingReport -> Bool
hasExplosion threshold tr = maxTermCount tr > threshold
