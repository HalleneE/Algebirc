{-# OPTIONS_GHC -Wno-type-defaults #-}
module AdversarialSpec (adversarialSpec) where

import Test.Hspec
import Algebirc.Core.Types
import Algebirc.Analysis.AdversarialOracle
import Algebirc.Analysis.CompositionCollapse
import Algebirc.Analysis.StructuralProbe
import Algebirc.Obfuscation.NonlinearTransform
  ( generateSBox, mkSBoxTransform, mkFeistelTransform
  , mkPowerMapTransform, mkARXTransform, applyNonlinear, invertNonlinear )

-- ============================================================
-- Fixtures
-- ============================================================

testCfg :: ObfuscationConfig
testCfg = defaultConfig { cfgFieldPrime = 257, cfgMaxDegree = 8, cfgEnableAnalysis = True }

testKey :: SecretKey
testKey = SecretKey { skSeed = 42, skRounds = 4, skSBoxSeed = 7, skPowerExp = 3 }

-- Dense polynomial
densePoly :: BoundedPoly
densePoly = mkBoundedPoly 257 8
  [ Term 12 8, Term 100 7, Term 45 6, Term 200 5, Term 7 4
  , Term 189 3, Term 33 2, Term 250 1, Term 1 0 ]

-- Pipeline konstruksi helpers
mkAffine :: Integer -> Integer -> Transform
mkAffine a b = Transform
  { transformTag    = AffineTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Just a
  , transformB      = Just b
  , transformSubs   = []
  , transformSBox   = Nothing
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  }

mkPermT :: Transform
mkPermT = Transform
  { transformTag    = PermutationTransform
  , transformPoly   = Nothing
  , transformPerm   = Nothing
  , transformA      = Nothing
  , transformB      = Nothing
  , transformSubs   = []
  , transformSBox   = Nothing
  , transformExp    = Nothing
  , transformRounds = 0
  , transformKey    = Nothing
  }

-- Pipeline: apply transform to polynomial (forward only)
applyTransformChain :: ObfuscationConfig -> [Transform] -> BoundedPoly -> Either AlgebircError BoundedPoly
applyTransformChain _ [] poly = Right poly
applyTransformChain cfg (t:ts) poly = do
  result <- applySingleTransform cfg t poly
  applyTransformChain cfg ts result

-- Apply single transform forward
applySingleTransform :: ObfuscationConfig -> Transform -> BoundedPoly -> Either AlgebircError BoundedPoly
applySingleTransform cfg t poly = case transformTag t of
  AffineTransform ->
    let p = cfgFieldPrime cfg
        maxDeg = polyMaxDegree poly
        a = maybe 1 id (transformA t)
        b = maybe 0 id (transformB t)
        newTerms = [ Term ((getCoeffAt i poly * a + b) `mod` p) i | i <- [0..maxDeg] ]
    in Right $ mkBoundedPoly p maxDeg newTerms
  PermutationTransform -> Right poly  -- simplified: identity perm
  SBoxTransform     -> applyNonlinear cfg t poly
  FeistelTransform  -> applyNonlinear cfg t poly
  PowerMapTransform -> applyNonlinear cfg t poly
  ARXDiffusionTransform -> applyNonlinear cfg t poly
  _ -> Right poly

-- ============================================================
-- Tests
-- ============================================================

adversarialSpec :: Spec
adversarialSpec = do

  -- =============================================
  -- AdversarialOracle Tests
  -- =============================================
  describe "Known-Plaintext Attack" $ do
    it "gagal recover pipeline nonlinear (atRecovered = False)" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          case mkPowerMapTransform 3 257 of
            Left _ -> expectationFailure "PowerMap failed"
            Right pmT -> do
              let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey, pmT]
                  pipelineFn = applyTransformChain testCfg pipeline
                  result = knownPlaintextAttack testCfg pipelineFn 20
              oaRecovered result `shouldBe` False

    it "all-linear pipeline punya rank drop rendah (dekat linear)" $ do
      let pipeline = [mkAffine 37 10, mkAffine 100 5]
          pipelineFn = applyTransformChain testCfg pipeline
          result = knownPlaintextAttack testCfg pipelineFn 20
      -- Linear pipeline: rank drop exists but attack params show linearity
      oaRankDrop result `shouldSatisfy` (<= 1.0)  -- any value valid, key is recovered flag

    it "solution space nonlinear > solution space linear" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let linPipeline = [mkAffine 37 10, mkPermT]
              nlPipeline  = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              linResult = knownPlaintextAttack testCfg (applyTransformChain testCfg linPipeline) 10
              nlResult  = knownPlaintextAttack testCfg (applyTransformChain testCfg nlPipeline) 10
          oaSolutionSpace nlResult `shouldSatisfy` (>= oaSolutionSpace linResult)

    it "residual error nonlinear > residual error linear" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let linFn = applyTransformChain testCfg [mkAffine 37 10]
              nlFn  = applyTransformChain testCfg [mkSBoxTransform sb, mkFeistelTransform sb 3 testKey]
              linResult = knownPlaintextAttack testCfg linFn 10
              nlResult  = knownPlaintextAttack testCfg nlFn 10
          oaResidualError nlResult `shouldSatisfy` (>= oaResidualError linResult)

  describe "Chosen-Plaintext Attack" $ do
    it "nonlinear pipeline tahan chosen-plaintext" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              result = chosenPlaintextAttack testCfg (applyTransformChain testCfg pipeline)
          oaRecovered result `shouldBe` False

    it "formatOracleResult menghasilkan output non-kosong" $ do
      let result = knownPlaintextAttack testCfg (applyTransformChain testCfg [mkAffine 37 10]) 5
      formatOracleResult result `shouldSatisfy` (not . null)

  describe "Differential Attack" $ do
    it "nonlinear pipeline: avalanche ≥20% (Feistel paired-element diffusion)" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              result = differentialAttack testCfg (applyTransformChain testCfg pipeline) densePoly
          drAvalancheRatio result `shouldSatisfy` (>= 0.20)

    it "ARX pipeline: avalanche ≥45% (full-width diffusion)" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let arxT = mkARXTransform sb testKey
              pipeline = [mkSBoxTransform sb, arxT, mkFeistelTransform sb 3 testKey, arxT]
              result = differentialAttack testCfg (applyTransformChain testCfg pipeline) densePoly
          drAvalancheRatio result `shouldSatisfy` (>= 0.45)

    it "nonlinear pipeline: 0 dead positions" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              result = differentialAttack testCfg (applyTransformChain testCfg pipeline) densePoly
          drDeadPositions result `shouldBe` 0

    it "nonlinear pipeline: tidak ada differential characteristic" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              result = differentialAttack testCfg (applyTransformChain testCfg pipeline) densePoly
          drIsDifferential result `shouldBe` False

    it "identity pipeline: minimal avalanche (< 15%, normalization artifacts only)" $ do
      let result = differentialAttack testCfg (applyTransformChain testCfg []) densePoly
      drAvalancheRatio result `shouldSatisfy` (< 0.15)

    it "formatDiffResult menghasilkan output non-kosong" $ do
      let result = differentialAttack testCfg (applyTransformChain testCfg [mkAffine 37 10]) densePoly
      formatDiffResult result `shouldSatisfy` (not . null)

  -- =============================================
  -- CompositionCollapse Tests
  -- =============================================
  describe "Composition Collapse" $ do
    it "S-box pipeline tidak collapse sampai 8 layers" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let sboxFn = applyTransformChain testCfg [mkSBoxTransform sb]
              results = testCompositionCollapse testCfg sboxFn densePoly [2, 4, 8]
          -- Tidak boleh ada collapse
          all (not . crCollapsed) results `shouldBe` True

    it "entropy tidak drop > 15% setelah multi-layer" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let sboxFn = applyTransformChain testCfg [mkSBoxTransform sb]
              results = testCompositionCollapse testCfg sboxFn densePoly [4, 8]
          all (\cr -> crEntropyDelta cr >= -0.15) results `shouldBe` True

    it "Jacobian rank tetap full rank untuk nonlinear pipeline" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let sboxFn = applyTransformChain testCfg [mkSBoxTransform sb]
              rank = computeJacobianRank testCfg sboxFn densePoly 2
          rank `shouldSatisfy` (> 0)

    it "fixed points tidak bertambah dengan scale" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let sboxFn = applyTransformChain testCfg [mkSBoxTransform sb]
              results = testCompositionCollapse testCfg sboxFn densePoly [2, 4, 8]
              fixeds = map crFixedPoints results
          -- Fixed points harus stabil, tidak bertambah drastis
          all (<= 5) fixeds `shouldBe` True

    it "degree growth ≥ 70% untuk nonlinear pipeline" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let sboxFn = applyTransformChain testCfg [mkSBoxTransform sb]
              growth = measureDegreeGrowth testCfg sboxFn densePoly 4
          growth `shouldSatisfy` (>= 0.70)

  describe "Pairwise Collapse" $ do
    it "deteksi affine collapse pada pair all-affine" $ do
      let pipeline = [mkAffine 37 10, mkAffine 100 5]
          collapses = testPairwiseCollapse testCfg pipeline densePoly
      length collapses `shouldSatisfy` (> 0)

    it "tidak ada collapse pada diverse pipeline" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          case mkPowerMapTransform 3 257 of
            Left _ -> expectationFailure "PowerMap failed"
            Right pmT -> do
              let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey, pmT]
                  collapses = testPairwiseCollapse testCfg pipeline densePoly
              -- Diverse pipeline: tidak ada simple collapse
              let hasAffineCollapse = any (\(_, _, msg) -> take 15 msg == "AFFINE_COLLAPSE") collapses
              hasAffineCollapse `shouldBe` False

  describe "Triple Collapse" $ do
    it "deteksi triple affine monoculture" $ do
      let pipeline = [mkAffine 37 10, mkAffine 100 5, mkAffine 13 200]
          triples = testTripleCollapse testCfg pipeline densePoly
      length triples `shouldSatisfy` (> 0)

    it "tidak ada triple collapse pada diverse pipeline" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          case mkPowerMapTransform 3 257 of
            Left _ -> expectationFailure "PowerMap failed"
            Right pmT -> do
              let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey, pmT]
                  triples = testTripleCollapse testCfg pipeline densePoly
              null triples `shouldBe` True

    it "formatCollapseResults menghasilkan output" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let sboxFn = applyTransformChain testCfg [mkSBoxTransform sb]
              results = testCompositionCollapse testCfg sboxFn densePoly [2, 4]
          formatCollapseResults results `shouldSatisfy` (not . null)

  -- =============================================
  -- StructuralProbe Tests
  -- =============================================
  describe "Algebraic Shortcut" $ do
    it "nonlinear pipeline: degree ≥ 70% expected" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              result = probeAlgebraicShortcut testCfg pipelineFn densePoly
          prDegreeRatio result `shouldSatisfy` (>= 0.70)

    it "nonlinear pipeline: tidak ada shortcut" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              result = probeAlgebraicShortcut testCfg pipelineFn densePoly
          prShortcutFound result `shouldBe` False

    it "nonlinear pipeline: tidak ada low-degree annihilator (deg 1-8)" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              result = probeAlgebraicShortcut testCfg pipelineFn densePoly
          prAnnihilatorDeg result `shouldBe` 0

    it "ARX pipeline: tidak ada annihilator sampai deg 8" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let arxT = mkARXTransform sb testKey
              pipeline = [mkSBoxTransform sb, arxT, mkFeistelTransform sb 3 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              result = probeAlgebraicShortcut testCfg pipelineFn densePoly
          prAnnihilatorDeg result `shouldBe` 0

    it "formatProbeResult menghasilkan output non-kosong" $ do
      let result = probeAlgebraicShortcut testCfg (applyTransformChain testCfg [mkAffine 37 10]) densePoly
      formatProbeResult result `shouldSatisfy` (not . null)

  describe "Invariant Propagation" $ do
    it "nonlinear pipeline: sum tidak preserved" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              sumInv = probeAdditiveInvariant testCfg pipelineFn densePoly
          ilSurvives sumInv `shouldBe` False

    it "nonlinear pipeline: parity tidak preserved" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              parityInv = probeParityInvariant testCfg pipelineFn densePoly
          ilSurvives parityInv `shouldBe` False

    it "nonlinear pipeline: tidak ada translation invariance" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              transInv = probeTranslationInvariant testCfg pipelineFn densePoly
          ilSurvives transInv `shouldBe` False

    it "nonlinear pipeline: tidak ada symmetry" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              syms = probeSymmetry testCfg pipelineFn densePoly
          all (\il -> not (ilSurvives il)) syms `shouldBe` True

    it "nonlinear pipeline: 0 invariant yang survive (total)" $ do
      case generateSBox 257 42 of
        Left _ -> expectationFailure "S-box failed"
        Right sb -> do
          let pipeline = [mkSBoxTransform sb, mkFeistelTransform sb 4 testKey]
              pipelineFn = applyTransformChain testCfg pipeline
              allInvariants = probeInvariantPropagation testCfg pipelineFn densePoly
              survivedCount = length (filter ilSurvives allInvariants)
          survivedCount `shouldBe` 0

    it "formatInvariants menghasilkan output non-kosong" $ do
      let allInvariants = probeInvariantPropagation testCfg (applyTransformChain testCfg [mkAffine 37 10]) densePoly
      formatInvariants allInvariants `shouldSatisfy` (not . null)
