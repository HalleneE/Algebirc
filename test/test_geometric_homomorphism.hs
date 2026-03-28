import Algebirc.Core.Types
import Algebirc.Core.Polynomial
import Algebirc.Evaluator.ObfEval
import Algebirc.Geometry.HyperellipticCurve
import Algebirc.Obfuscation.Pipeline
import Algebirc.Evaluator.Eval (Expr(..), BinOp(..))
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Data.Vector as V

main :: IO ()
main = do
  let p = 257
      cfg = defaultConfig { cfgFieldPrime = p, cfgGenus = 2 }
      Right pl = buildPipeline cfg
      
      -- 1. Setup Curve & Points
      -- C: y^2 = x^5 + x + 1 (Typical genus-2)
      f = iToV [1, 1, 0, 0, 0, 1] 
      hc = HyperCurve f 2 p
      
      -- Create two points (Divisors)
      -- D1: u=x^2 + 2x + 3, v=0
      d1 = MumfordDiv (iToV [3, 2, 1]) (V.singleton 0) p
      -- D2: u=x^2 + 4x + 5, v=0
      d2 = MumfordDiv (iToV [5, 4, 1]) (V.singleton 0) p
      
      -- 2. Wrap into ObfuscatedValue (Geometric Mode)
      dummyG = d1
      dummyCtxs = (undefined, undefined) -- not used in add
      ov1 = ObfPoint d1 hc dummyG cfg pl dummyCtxs
      ov2 = ObfPoint d2 hc dummyG cfg pl dummyCtxs
      
      -- 3. Perform Homomorphic Addition
      env = Map.fromList [("a", ov1), ("b", ov2)]
      expr = BinOpExpr Add (Var "a") (Var "b")
      
  putStrLn "Testing Geometric Homomorphic Addition (Cantor's Algorithm)..."
  
  runExceptT (evalObf env expr) >>= \case
    Left err -> putStrLn $ "Error: " ++ show err
    Right (ObfPoint resDiv _ _ _ _ _) -> do
      putStrLn "Success! Result is a Jacobian Point."
      putStrLn $ "Result u(x): " ++ show (mdU resDiv)
      putStrLn $ "Result v(x): " ++ show (mdV resDiv)
      
      -- Verify with manual Cantor
      let expected = jacobianAdd hc d1 d2
      if mdU resDiv == mdU expected && mdV resDiv == mdV expected
        then putStrLn "VERIFIED: Eval result matches manual Cantor addition."
        else putStrLn "FAILURE: Result mismatch."
    _ -> putStrLn "Error: Result is not a point."
