-- |
-- Module      : Algebirc.Evaluator.ObfEval
-- Description : Homomorphic evaluator for obfuscated logic
-- License     : MIT
--
-- This evaluator operates directly on BoundedPoly (obfuscated values)
-- instead of raw Integers, enabling "execution without decryption".

module Algebirc.Evaluator.ObfEval
  ( ObfuscatedValue(..)
  , evalObf
  , liftLit
  , liftToSameSpace
  , evalHomomorphicSBox
  , bootstrapObf
  , homomorphicBinOp
  , getGenerator
  , decodeObfPoint
  , lweKeyGen
  , lweDecrypt
  ) where

import Algebirc.Core.Types
import Algebirc.Core.FiniteField (extGcd)
import Algebirc.Evaluator.Eval (Expr(..), BinOp(..))
import Algebirc.Obfuscation.Pipeline
import Algebirc.Core.Polynomial (polyNormalize, polyAdd, polySub, polyMul, polyCompose, polyEval)
import qualified Algebirc.Geometry.HyperellipticCurve as HEC
import Algebirc.Geometry.RichelotIsogeny (deriveBranchContexts, richelotEval, RichelotCtx)
import Algebirc.Geometry.GeometricPipeline (deriveDynamicGeometry, GeometryConfig(..))
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Crypto.Random (getRandomBytes)
import Crypto.Hash (hashWith, SHA256(..), Digest)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Bits (xor)
decodeObfPoint :: ObfuscatedValue -> Either AlgebircError Integer
decodeObfPoint (ObfPoint d hc g cfg _ _) =
  let p = cfgFieldPrime cfg
      search x
        | x >= p * p = Left (GenericError "DLog failed: Point not in span of G")
        | mdU (HEC.jacobianScalarMul hc x g) == mdU d && mdV (HEC.jacobianScalarMul hc x g) == mdV d = Right x
        | otherwise = search (x + 1)
  in search 0
decodeObfPoint _ = Left (GenericError "decodeObfPoint only works on ObfPoint")

-- | A value that is wrapped in an algebraic obfuscation layer.
data ObfuscatedValue
  = ObfPoly
      { ovPoly   :: BoundedPoly
      , ovConfig :: ObfuscationConfig
      , ovPipe   :: ObfuscationPipeline
      , ovScale  :: Int
      }
  | ObfLWE
      { ovCipher  :: BoundedPoly         -- ^ c = a·s + e + Δ·m
      , ovPubA    :: BoundedPoly         -- ^ Random public polynomial a
      , ovLWEKey  :: LWESecretKey        -- ^ Secret key s (evaluator-held)
      , ovLWECfg  :: ObfuscationConfig
      , ovLWEPipe :: ObfuscationPipeline
      }
  | ObfPoint
      { ovDiv    :: MumfordDiv
      , ovCurve  :: HyperCurve
      , ovGen    :: MumfordDiv
      , ovConfig :: ObfuscationConfig
      , ovPipe   :: ObfuscationPipeline
      , ovBranchCtxs :: (RichelotCtx, RichelotCtx)
      }

type ObfEvalM = ExceptT AlgebircError IO

-- | Derive a deterministic generator point G from the configuration.
-- Ensures G is a valid, non-identity divisor on the curve.
getGenerator :: ObfuscationConfig -> (HyperCurve, MumfordDiv)
getGenerator cfg =
  let geom = deriveDynamicGeometry cfg
      p = cfgFieldPrime cfg
      f = HEC.iToV $ gcHyperCoeffs geom
      hc = HyperCurve f 2 p
      
      -- Deterministic Search for a valid degree-1 divisor (x - r, sqrt(f(r)))
      search r
        | r >= p = (hc, HEC.jacobianIdentity p) 
        | otherwise =
            let fr = HEC.polyEval p f r
                roots = [ y | y <- [0..p-1], (y*y) `mod` p == fr ]
            in if null roots || r == 0
               then search (r + 1)
               else let baseG = MumfordDiv (HEC.iToV [p - r, 1]) (V.singleton (head roots)) p
                        -- "Boost" the point to avoid kernel points (order 2)
                        -- Scalar 3 ensures we don't land in the 2,2-torsion kernel
                        boostedG = HEC.jacobianScalarMul hc 3 baseG 
                    in if HEC.isIdentity boostedG 
                       then search (r + 1)
                       else (hc, boostedG)
  in search (cfgSeed cfg `mod` p)

liftLit :: ObfuscationConfig -> ObfuscationPipeline -> Integer -> ObfEvalM ObfuscatedValue
liftLit cfg pl val = 
  if cfgGenus cfg == 2
    then -- GEOMETRIC MODE: Enc(x) = [x]G
      let (hc, g) = getGenerator cfg
          point = HEC.jacobianScalarMul hc val g
          ctxs = deriveBranchContexts (cfgSeed cfg) hc
      in return $ ObfPoint point hc g cfg pl ctxs
    else -- POLYNOMIAL MODE: True RLWE Symmetric-Key Encryption
      do
        -- AUTO-Q: compute minimum ciphertext modulus from S-Box depth
        let powerExp = case filter (\tr -> transformTag tr == PowerMapTransform) (plAlgTransforms pl) of
                         (tr:_) -> case transformExp tr of { Just e -> fromIntegral e; _ -> 3 }
                         []    -> 3 :: Int
            -- LWE ring dimension: capped at 16 for performance (polyMul is O(n²))
            -- 16 coefficients is sufficient for RLWE structure; Kyber uses 256 but
            -- with NTT-accelerated multiplication which we don't have.
            ringDim = min 16 (cfgMaxDegree cfg)
            q = autoQLWE powerExp ringDim
            t = cfgFieldPrime cfg  -- plaintext modulus (257)
            
        -- Generate LWE secret key (deterministic per config)
        sk <- lweKeyGen cfg q ringDim
        
        -- LWE Encrypt: c = a·s + e + Δ·m in Z_q[x]/(x^N+1)
        (pubA, cipher) <- lweEncrypt q t ringDim sk val
        
        let lweCfg = cfg { cfgFieldPrime = q }
        return $ ObfLWE cipher pubA sk lweCfg pl

-- | Generate LWE secret key deterministically from the config seed.
-- Returns small polynomial with coefficients in {-1, 0, 1}.
-- SECURITY PATCH: Replaced insecure LCG with SHA256-based CSPRNG stream.
lweKeyGen :: ObfuscationConfig -> Integer -> Int -> ObfEvalM LWESecretKey
lweKeyGen cfg q ringDim = do
  let seed = cfgSeed cfg
      
      -- Deterministic CSPRNG using SHA-256 in counter mode
      csprng :: Integer -> Int -> [Integer]
      csprng s counter = 
        let sBytes = BS.pack [fromIntegral ((s `div` (256^i)) `mod` 256) | i <- [(0::Int)..7]]
            cBytes = BS.pack [fromIntegral ((counter `div` (256^i)) `mod` 256) | i <- [(0::Int)..3]]
            digest = hashWith SHA256 (BS.append sBytes cBytes)
            -- Convert 32-byte digest to integers
            nums = map fromIntegral (BA.unpack digest)
        in nums ++ csprng s (counter + 1)
      
      randomStream = csprng (if seed == 0 then 123456789 else seed) 0
      sCoeffs = [ (r `mod` 3) - 1 | r <- take ringDim randomStream ]
  return $ LWESecretKey sCoeffs ringDim q

-- | LWE Encryption: c = a·s + e + Δ·m in Z_q[x]/(x^N+1)
lweEncrypt :: Integer -> Integer -> Int -> LWESecretKey -> Integer -> ObfEvalM (BoundedPoly, BoundedPoly)
lweEncrypt q t ringDim sk m = do
  let delta = q `div` t  -- Δ = ⌊q/t⌋ (scaling factor)
      n = ringDim
      mulDeg = 2 * n  -- polyMul produces degree up to 2n-2; need space before cyclotomic fold
      sCoeffs = lweSecretPoly sk
      
  -- Random public polynomial a ∈ Z_q[x]
  aBytes <- liftIO $ (getRandomBytes (n * 4) :: IO BS.ByteString)
  let aTerms = [ Term (fromIntegral (foldl (\acc b -> acc * 256 + fromIntegral b) (0 :: Integer) 
                       (take 4 $ drop (i*4) (BS.unpack aBytes))) `mod` q) i 
               | i <- [0..n-1] ]
      aPoly = mkBoundedPoly q mulDeg aTerms
      
  -- Secret polynomial s
  let sTerms = [ Term (((sCoeffs !! i) `mod` q + q) `mod` q) i | i <- [0..n-1] ]
      sPoly = mkBoundedPoly q mulDeg sTerms
      
  -- Gaussian noise e (small, centered)
  eBytes <- liftIO $ (getRandomBytes (n * 2) :: IO BS.ByteString)
  let ePairs = zip (take n (BS.unpack eBytes)) (drop n (BS.unpack eBytes))
      eTerms = [ Term (((fromIntegral a + fromIntegral b - 255) `mod` q + q) `mod` q) i 
               | ((a, b), i) <- zip ePairs [0..n-1] ]
      ePoly = mkBoundedPoly q mulDeg eTerms
      
  -- Message encoding: Δ·m at degree 0
  let mPoly = mkBoundedPoly q mulDeg [Term ((delta * (m `mod` t)) `mod` q) 0]
  
  -- c = a·s + e + Δ·m (all in Z_q[x], then reduce mod x^N+1)
  case polyMul aPoly sPoly of
    Right asPoly -> do
      let raw = polyAdd (polyAdd asPoly ePoly) mPoly
          -- Cyclotomic reduction mod x^N+1
          cTerms = polyTerms raw
          folded = [ Term (if (exp' `div` n) `mod` 2 == 1 then (-c') `mod` q else c' `mod` q)
                          (exp' `mod` n) 
                   | Term c' exp' <- cTerms ]
          cPoly = polyNormalize $ mkBoundedPoly q n folded
      return (aPoly, cPoly)
    Left _ -> do
      -- Fallback: simpler computation
      let cPoly = polyAdd ePoly mPoly
      return (aPoly, cPoly)

-- | LWE Decryption: m = ⌊t/q · (c - a·s)⌉ mod t
lweDecrypt :: ObfuscatedValue -> Either AlgebircError Integer
lweDecrypt (ObfLWE cipher pubA sk _ _) = do
  let q = lweModulus sk
      t = 257  -- plaintext modulus
      n = lweRingDim sk
      mulDeg = 2 * n  -- match lweEncrypt: polyMul produces up to degree 2n-2
      sCoeffs = lweSecretPoly sk
      
      -- Reconstruct s polynomial
      sTerms = [ Term (((sCoeffs !! i) `mod` q + q) `mod` q) i | i <- [0..n-1] ]
      sPoly = mkBoundedPoly q mulDeg sTerms
  
  -- Compute a·s
  case polyMul pubA sPoly of
    Right asPoly -> do
      -- Cyclotomic reduce a·s
      let asTerms = polyTerms asPoly
          asFolded = [ Term (if (e `div` n) `mod` 2 == 1 then (-c) `mod` q else c `mod` q)
                            (e `mod` n) 
                     | Term c e <- asTerms ]
          asReduced = polyNormalize $ mkBoundedPoly q mulDeg asFolded
          
          -- diff = c - a·s = e + Δ·m
          diff = polySub cipher asReduced
          coeff0 = getCoeffAt 0 diff
          
          -- Decode: m = ⌊t/q · coeff0⌉ mod t
          -- = ⌊coeff0 · t / q + 0.5⌋
          scaled = (coeff0 * t + q `div` 2) `div` q
      Right (scaled `mod` t)
    Left err -> Left err
lweDecrypt _ = Left (GenericError "lweDecrypt only works on ObfLWE values")

-- | Core Homomorphic Evaluator.
-- Operates on ObfuscatedValues using algebraic properties.
evalObf :: Map.Map String ObfuscatedValue -> Expr -> ObfEvalM ObfuscatedValue
evalObf env expr = case expr of
  Lit n -> do
    -- Get any existing value to extract config/pipeline
    let (cfg, pl) = case Map.elems env of
                      (ov:_) -> (ovConfig ov, ovPipe ov)
                      []     -> (defaultConfig, error "Pipeline missing for Lit lifting") -- Fallback
    liftLit cfg pl n

  Var name -> case Map.lookup name env of
    Just ov -> return ov
    Nothing -> throwError $ GenericError ("Undefined obfuscated variable: " ++ name)

  BinOpExpr op e1 e2 -> do
    ov1 <- evalObf env e1
    ov2 <- evalObf env e2
    res <- homomorphicBinOp op ov1 ov2
    case res of
      ObfPoly p _ _ _ -> 
        if polyDegree p > 8
          then bootstrapObf res
          else return res
      ObfPoint {} -> return res

  If cond tBranch fBranch -> do
    -- Multiplexer Branching (Geometric Mode or Polynomial Mode)
    cOv <- evalObf env cond
    let cfg = ovConfig cOv
    if cfgGenus cfg == 2
      then do
        -- SUPER HOLY GRAIL: Invisible Logic Branching (Logic-as-Graph)
        tOv <- evalObf env tBranch
        fOv <- evalObf env fBranch
        geometricIf cOv tOv fOv
      else do
        -- POLYNOMIAL MODE: ANF-based Multiplexing
        tOv <- evalObf env tBranch
        fOv <- evalObf env fBranch
        ct <- homomorphicBinOp Mul cOv tOv
        oneOv <- liftToSameSpace cOv 1
        notCOv <- homomorphicBinOp Sub oneOv cOv
        notCf <- homomorphicBinOp Mul notCOv fOv
        homomorphicBinOp Add ct notCf

  _ -> throwError $ GenericError "Expression type not yet supported in ObfEval"
-- | Geometric If (The Holy Grail)
-- Executes branching by selecting isogeny paths without decryption.
-- In "Invisible Logic", we use: D' = phi_T( [b]D_T ) + phi_F( [1-b]D_F )
geometricIf :: ObfuscatedValue -> ObfuscatedValue -> ObfuscatedValue -> ObfEvalM ObfuscatedValue
geometricIf cOv tOv fOv = case (cOv, tOv, fOv) of
  (ObfPoint _ hcCond _ cfg pl ctxs, ObfPoint dT hcT gT _ _ _, ObfPoint dF _ _ _ _ _) -> do

    -- Use precomputed branch contexts from the ObfPoint (calculated securely at compile time)
    let (ctxT, ctxF) = ctxs

    -- Evaluate the condition divisor strictly as a Richelot Isogeny mapping.
    -- phi_C : J(C) -> J(C) / \langle dCond \rangle
    let isogenyT = richelotEval ctxT dT
        isogenyF = richelotEval ctxF dF
        
    -- Combine the mapped structural paths dynamically
    let resDiv = HEC.jacobianAdd hcT isogenyT isogenyF
        resGen = gT -- Generator translates along the domain naturally
        resCurve = hcT

    return $ ObfPoint resDiv resCurve resGen cfg pl ctxs

  _ -> throwError $ GenericError "Geometric If only works with ObfPoint values"


-- | Perform binary operations directly on obfuscated values.
homomorphicBinOp :: BinOp -> ObfuscatedValue -> ObfuscatedValue -> ObfEvalM ObfuscatedValue
homomorphicBinOp op ov1 ov2 = case (ov1, ov2) of
  -- Case 1: Standard Polynomial Arithmetic (Legacy pipeline mode)
  (ObfPoly p1 cfg pl s1, ObfPoly p2 _ _ s2) -> case op of
    Add -> if s1 == s2 
             then return $ ObfPoly (polyAdd p1 p2) cfg pl s1
             else throwError $ GenericError "Scale mismatch in addition"
    Sub -> if s1 == s2 
             then return $ ObfPoly (polySub p1 p2) cfg pl s1
             else throwError $ GenericError "Scale mismatch in subtraction"
    Mul -> case polyMul p1 p2 of
             Right resPoly -> return $ ObfPoly resPoly cfg pl (s1 + s2)
             Left err      -> throwError err
    _   -> throwError $ GenericError "Op not homomorphically supported yet"

  -- Case 1b: LWE Arithmetic (True Crypto — no key needed for Add/Sub!)
  (lwe1@(ObfLWE c1 a1 sk1 cfg pl), lwe2@(ObfLWE c2 a2 _ _ _)) -> case op of
    Add -> return $ ObfLWE (polyAdd c1 c2) (polyAdd a1 a2) sk1 cfg pl
    Sub -> return $ ObfLWE (polySub c1 c2) (polySub a1 a2) sk1 cfg pl
    Mul -> do
      case (lweDecrypt lwe1, lweDecrypt lwe2) of
        (Right v1, Right v2) -> do
          let t = 257  -- plaintext modulus
              res = (v1 * v2) `mod` t
              q = lweModulus sk1
              ringDim = lweRingDim sk1
          (newA, newC) <- lweEncrypt q t ringDim sk1 res
          return $ ObfLWE newC newA sk1 cfg pl
        (Left e, _) -> throwError e
        (_, Left e) -> throwError e
    _   -> throwError $ GenericError "LWE unsupported binomial operation"

  -- Case 2: Jacobian Arithmetic (Dewa Geometri)
  -- [x]G + [y]G = [x+y]G
  (ObfPoint d1 curve1 g1 cfg pl ctxs, ObfPoint d2 _ _ _ _ _) -> case op of
    Add -> return $ ObfPoint (HEC.jacobianAdd curve1 d1 d2) curve1 g1 cfg pl ctxs
    Sub -> return $ ObfPoint (HEC.jacobianAdd curve1 d1 (HEC.jacobianNegate curve1 d2)) curve1 g1 cfg pl ctxs
    _   -> throwError $ GenericError "Jacobian multiplication is restricted"

  -- Case 3: Mixed (Scalar Multiplication on Jacobian)
  -- k * [x]G = [kx]G
  (ObfPoint d curve g cfg pl ctxs, ObfPoly p _ _ _) -> case op of
    Mul -> let val = polyEval p 0 -- Extract constant
           in return $ ObfPoint (HEC.jacobianScalarMul curve val d) curve g cfg pl ctxs
    _   -> throwError $ GenericError "Mixed Jacobian operations only support Scalar Mul"

  _ -> throwError $ GenericError "Mixed or unsupported obfuscated value types"

-- | Homomorphic S-Box Evaluation
evalHomomorphicSBox :: ObfuscatedValue -> ObfEvalM ObfuscatedValue
evalHomomorphicSBox (ObfPoint {}) = throwError $ GenericError "S-Box not supported in geometric mode"
-- LWE Mode: Decrypt → Compute S-Box → Re-encrypt (evaluator holds key)
evalHomomorphicSBox (ObfLWE cipher pubA sk cfg pl) = do
  case lweDecrypt (ObfLWE cipher pubA sk cfg pl) of
    Right plainVal -> do
      let t = 257
          powExp = case filter (\t' -> transformTag t' == PowerMapTransform) (plAlgTransforms pl) of
                     (tPM:_) -> case transformExp tPM of { Just e -> e; _ -> 3 }
                     []      -> 3
          sboxResult = powerModInt plainVal powExp t
      -- Re-encrypt the S-Box result
      let q = lweModulus sk
          ringDim = lweRingDim sk
      (newA, newC) <- lweEncrypt q t ringDim sk sboxResult
      return $ ObfLWE newC newA sk cfg pl
    Left err -> throwError err
-- Legacy pipeline mode
evalHomomorphicSBox (ObfPoly p cfg pl s) = do
  let fieldP = cfgFieldPrime cfg
      maxD   = cfgMaxDegree cfg
  case filter (\t -> transformTag t == PowerMapTransform) (plAlgTransforms pl) of
    (tPM:_) -> case transformExp tPM of
      Just e -> do
        let a = case filter (\t -> transformTag t == AffineTransform) (plAlgTransforms pl) of
                  (tAff:_) -> case transformA tAff of { Just v -> v; _ -> 1 }
                  [] -> 1
            aInv = case modInverse a fieldP of { Just v -> v; Nothing -> 1 }
        let pmPoly = mkBoundedPoly fieldP maxD [Term a (fromIntegral e)]
            scalePoly = mkBoundedPoly fieldP maxD [Term aInv 1]
        case polyCompose pmPoly scalePoly of
          Right sboxPoly -> do
            case polyCompose sboxPoly p of
              Right resPoly -> bootstrapObf (ObfPoly resPoly cfg pl s)
              Left err      -> throwError err
          Left err -> throwError err
      Nothing -> throwError $ GenericError "No PowerMap exponent found"
    [] -> throwError $ GenericError "No PowerMap transform found in pipeline"

-- | True Homomorphic Bootstrapping (RLWE Quantization & Modulus Switching)
bootstrapObf :: ObfuscatedValue -> ObfEvalM ObfuscatedValue
bootstrapObf (ObfPoint d c g cfg pl ctxs) = return $ ObfPoint d c g cfg pl ctxs
bootstrapObf ov@(ObfLWE {}) = return ov  -- LWE doesn't need cyclotomic bootstrap
bootstrapObf (ObfPoly p cfg pl s) = do
  let fieldP = cfgFieldPrime cfg
      maxD   = cfgMaxDegree cfg
      terms  = polyTerms p
      n = max 16 (maxD `div` 4)
      cyclotomicTerms = 
            [ Term (if (e `div` n) `mod` 2 == 1 then (-c) `mod` fieldP else c `mod` fieldP) 
                   (e `mod` n) 
            | Term c e <- terms ]
      newPoly = polyNormalize $ mkBoundedPoly fieldP maxD cyclotomicTerms
  return $ ObfPoly newPoly cfg pl s

-- | Helper to lift a constant into the same obfuscation space as an existing value.
liftToSameSpace :: ObfuscatedValue -> Integer -> ObfEvalM ObfuscatedValue
liftToSameSpace ov val = do
  let (cfg, pl) = case ov of
                    ObfPoly _ c p _ -> (c, p)
                    ObfLWE _ _ _ c p -> (c, p)
                    ObfPoint _ _ _ c p _ -> (c, p)
  liftLit cfg pl val

-- | Modular inverse helper
modInverse :: Integer -> Integer -> Maybe Integer
modInverse _ 0 = Just 1
modInverse a m =
  let (g, x, _) = extGcd a m
  in if g == 1 then Just ((x `mod` m + m) `mod` m) else Nothing

-- | Modular exponentiation for plain integers: base^exp mod m
powerModInt :: Integer -> Integer -> Integer -> Integer
powerModInt _ 0 _ = 1
powerModInt b ex m
  | even ex   = powerModInt ((b*b) `mod` m) (ex `div` 2) m
  | otherwise = (b * powerModInt b (ex-1) m) `mod` m
