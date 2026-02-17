-- |
-- Module      : Algebirc.Ordering.Scheduler
-- Description : File composition graph and execution scheduling
-- License     : MIT
--
-- Manages the dependency graph between obfuscated modules and
-- determines execution order based on SHA-256 fingerprints and
-- mathematical priority computation.

module Algebirc.Ordering.Scheduler
  ( -- * Execution Graph
    ExecutionGraph(..)
  , ExecutionNode(..)
  , buildExecutionGraph
    -- * Scheduling
  , schedule
  , ScheduleResult(..)
    -- * Visualization
  , showSchedule
  ) where

import Algebirc.Core.Types
import Algebirc.Ordering.Fingerprint
import Algebirc.Ordering.MathOrder
import Algebirc.Obfuscation.Encoder (EncodedModule(..))
import Data.List (sortBy, intercalate)
import Data.Ord (comparing)

-- ============================================================
-- Types
-- ============================================================

-- | A node in the execution graph.
data ExecutionNode = ExecutionNode
  { enModule      :: !EncodedModule
  , enFingerprint :: !FileFingerprint
  , enPriority    :: !Integer
  , enDeps        :: ![String]     -- ^ Module dependencies (by name)
  } deriving (Show, Eq)

-- | The complete execution graph.
data ExecutionGraph = ExecutionGraph
  { egNodes     :: ![ExecutionNode]
  , egConfig    :: !ObfuscationConfig
  } deriving (Show, Eq)

-- | Result of scheduling: ordered execution plan.
data ScheduleResult = ScheduleResult
  { srOrder     :: ![ExecutionNode]    -- ^ Nodes in execution order
  , srConfig    :: !ObfuscationConfig
  , srPhases    :: ![[ExecutionNode]]  -- ^ Grouped by dependency level
  } deriving (Show, Eq)

-- ============================================================
-- Graph Construction
-- ============================================================

-- | Build an execution graph from a list of encoded modules.
-- Dependencies are specified as (module, [dependency names]).
buildExecutionGraph :: ObfuscationConfig -> [(EncodedModule, [String])] -> ExecutionGraph
buildExecutionGraph cfg modules =
  let nodes = map (\(em, deps) ->
        let fp = computeFingerprint em
            priority = computePriority cfg fp
        in ExecutionNode
             { enModule      = em
             , enFingerprint = fp
             , enPriority    = priority
             , enDeps        = deps
             }) modules
  in ExecutionGraph
       { egNodes  = nodes
       , egConfig = cfg
       }

-- ============================================================
-- Scheduling
-- ============================================================

-- | Schedule execution order.
--
-- Algorithm:
-- 1. Topological sort based on dependencies
-- 2. Within each dependency level, order by mathematical priority
-- 3. This gives a deterministic execution order based on content hash
schedule :: ExecutionGraph -> ScheduleResult
schedule eg =
  let cfg    = egConfig eg
      nodes  = egNodes eg
      -- Simple topological sort with priority ordering within levels
      phases = topoSort nodes
      order  = concatMap (sortBy (comparing enPriority)) phases
  in ScheduleResult
       { srOrder  = order
       , srConfig = cfg
       , srPhases = phases
       }

-- | Topological sort: group nodes into levels by dependency depth.
topoSort :: [ExecutionNode] -> [[ExecutionNode]]
topoSort [] = []
topoSort nodes =
  let -- Find nodes with no unsatisfied dependencies
      resolved = map (emName . enModule) $ concatMap id (topoSort' nodes [])
      -- Actually, let's do a simpler iterative approach
  in topoSort' nodes []

topoSort' :: [ExecutionNode] -> [String] -> [[ExecutionNode]]
topoSort' [] _ = []
topoSort' remaining resolved =
  let -- Find nodes whose deps are all resolved
      (ready, notReady) = partition' (\n -> all (`elem` resolved) (enDeps n)) remaining
  in if null ready
     then [remaining]  -- cycle detected or unresolvable: just dump remaining
     else let newResolved = resolved ++ map (emName . enModule) ready
          in ready : topoSort' notReady newResolved

-- | Simple partition (avoiding Data.List.partition for clarity).
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' p (x:xs) =
  let (yes, no) = partition' p xs
  in if p x then (x:yes, no) else (yes, x:no)

-- ============================================================
-- Visualization
-- ============================================================

-- | Pretty-print the execution schedule.
showSchedule :: ScheduleResult -> String
showSchedule sr =
  let header = "╔══════════════════════════════════════════════════╗\n" ++
               "║         Execution Schedule                     ║\n" ++
               "╚══════════════════════════════════════════════════╝\n"
      phases = zipWith showPhase [1..] (srPhases sr)
  in header ++ "\n" ++ intercalate "\n" phases

showPhase :: Int -> [ExecutionNode] -> String
showPhase level nodes =
  "━━━ Phase " ++ show level ++ " ━━━\n" ++
  unlines (map showNode nodes)

showNode :: ExecutionNode -> String
showNode en =
  "  [" ++ take 8 (fpHashHex (enFingerprint en)) ++ "..] " ++
  emName (enModule en) ++
  " (priority=" ++ show (enPriority en) ++ ")" ++
  if null (enDeps en)
  then ""
  else " depends: [" ++ intercalate ", " (enDeps en) ++ "]"
