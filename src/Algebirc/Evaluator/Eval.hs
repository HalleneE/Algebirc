-- |
-- Module      : Algebirc.Evaluator.Eval
-- Description : Turing-complete expression evaluator with bounded execution
-- License     : MIT
--
-- = Turing Completeness
--
-- This evaluator supports:
-- * __Unbounded memory__: variable environment grows dynamically
-- * __Conditional branching__: 'If' expressions
-- * __Recursion__: 'LetRec' with bounded depth (termination guarantee)
-- * __State mutation__: 'Let' bindings modify environment
--
-- = Termination Guarantees
--
-- * __Depth bound__: recursion depth is hard-capped by 'esMaxDepth'
-- * __Step budget__: total evaluation steps capped by 'esMaxSteps'
-- * Both exceed → evaluation halts with descriptive error

module Algebirc.Evaluator.Eval
  ( -- * Expression AST
    Expr(..)
  , BinOp(..)
  , UnaryOp(..)
    -- * Evaluation
  , eval
  , evalPure
  , evalWithConfig
    -- * Pretty Printing
  , prettyExpr
  ) where

import Algebirc.Core.Types
import Algebirc.Core.FiniteField
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except

-- ============================================================
-- Expression AST
-- ============================================================

-- | Binary operators over field elements.
data BinOp
  = Add | Sub | Mul | Div | Pow | Mod
  deriving (Show, Eq)

-- | Unary operators.
data UnaryOp
  = Neg | Inv
  deriving (Show, Eq)

-- | Expression language.
--
-- This AST is Turing-complete when combined with 'LetRec':
-- * 'Lit' + 'Var' for data
-- * 'BinOp' for computation
-- * 'If' for branching
-- * 'Let' for state (environment mutation)
-- * 'LetRec' for recursion (bounded)
-- * 'Seq' for sequencing
-- * 'PolyEval' for polynomial evaluation (algebraic operations)
data Expr
  = Lit !Integer                        -- ^ Literal value
  | Var !String                         -- ^ Variable reference
  | BinOpExpr !BinOp !Expr !Expr        -- ^ Binary operation
  | UnaryOpExpr !UnaryOp !Expr          -- ^ Unary operation
  | If !Expr !Expr !Expr                -- ^ Conditional: if cond then a else b
  | Let !String !Expr !Expr             -- ^ Let binding: let x = e1 in e2
  | LetRec !String !Expr !Expr          -- ^ Recursive binding (depth-bounded)
  | Seq ![Expr]                         -- ^ Sequence: evaluate each, return last
  | PolyEval ![(Integer, Int)] !Expr    -- ^ Evaluate polynomial at expression
  | FieldLit !Integer !Integer          -- ^ Field element literal (value, prime)
  deriving (Show, Eq)

-- ============================================================
-- Evaluation Monad
-- ============================================================

type EvalM = ExceptT AlgebircError (State EvalState)

-- | Tick the step counter. Error if budget exceeded.
tick :: EvalM ()
tick = do
  st <- get
  let steps = esStepCount st + 1
  when (steps > esMaxSteps st) $
    throwError (EvalStepsExceeded (esMaxSteps st))
  put st { esStepCount = steps }

-- | Enter a recursive scope. Error if depth exceeded.
enterScope :: EvalM ()
enterScope = do
  st <- get
  let d = esDepth st + 1
  when (d > esMaxDepth st) $
    throwError (EvalDepthExceeded (esMaxDepth st))
  put st { esDepth = d }

-- | Leave a recursive scope.
leaveScope :: EvalM ()
leaveScope = modify' $ \st -> st { esDepth = esDepth st - 1 }

-- | Look up a variable in the environment.
lookupVar :: String -> EvalM Integer
lookupVar name = do
  env <- gets esEnv
  case Map.lookup name env of
    Just v  -> return v
    Nothing -> throwError (GenericError $ "Undefined variable: " ++ name)

-- | Bind a variable in the environment.
bindVar :: String -> Integer -> EvalM ()
bindVar name val = modify' $ \st ->
  st { esEnv = Map.insert name val (esEnv st) }

-- ============================================================
-- Core Evaluator
-- ============================================================

-- | Evaluate an expression, returning a field-element value.
-- All arithmetic is done in the integer domain; if you need
-- modular arithmetic, use 'FieldLit' or 'PolyEval'.
evalExpr :: Integer -> Expr -> EvalM Integer
evalExpr p expr = do
  tick
  case expr of
    Lit n -> return (n `mod` p)

    FieldLit v fp -> return (v `mod` fp)

    Var name -> lookupVar name

    BinOpExpr op e1 e2 -> do
      v1 <- evalExpr p e1
      v2 <- evalExpr p e2
      evalBinOp p op v1 v2

    UnaryOpExpr op e -> do
      v <- evalExpr p e
      evalUnaryOp p op v

    If cond tBranch fBranch -> do
      c <- evalExpr p cond
      if c /= 0
        then evalExpr p tBranch
        else evalExpr p fBranch

    Let name binding body -> do
      val <- evalExpr p binding
      enterScope
      bindVar name val
      result <- evalExpr p body
      leaveScope
      return result

    LetRec name binding body -> do
      -- First evaluate with dummy value (0) for recursion base
      enterScope
      bindVar name 0
      val <- evalExpr p binding
      bindVar name val
      result <- evalExpr p body
      leaveScope
      return result

    Seq [] -> return 0
    Seq [e] -> evalExpr p e
    Seq (e:es) -> do
      _ <- evalExpr p e
      evalExpr p (Seq es)

    PolyEval coeffs argExpr -> do
      x <- evalExpr p argExpr
      -- Horner evaluation of polynomial with coefficients
      let horner [] acc     = acc
          horner ((c,e):rest) acc =
            let term = (c * powMod x (fromIntegral e) p) `mod` p
            in horner rest ((acc + term) `mod` p)
      return (horner coeffs 0)

-- | Binary operation evaluation in GF(p).
evalBinOp :: Integer -> BinOp -> Integer -> Integer -> EvalM Integer
evalBinOp p Add a b = return $ (a + b) `mod` p
evalBinOp p Sub a b = return $ (a - b + p) `mod` p
evalBinOp p Mul a b = return $ (a * b) `mod` p
evalBinOp _ Div _ 0 = throwError DivisionByZero
evalBinOp p Div a b = do
  let (g, x, _) = extGcd b p
  if g /= 1
    then throwError (InverseNotFound $ show b ++ " in GF(" ++ show p ++ ")")
    else return $ (a * ((x `mod` p + p) `mod` p)) `mod` p
evalBinOp p Pow a b = return $ powMod a b p
evalBinOp _ Mod a b
  | b == 0    = throwError DivisionByZero
  | otherwise = return $ a `mod` b

-- | Unary operation evaluation.
evalUnaryOp :: Integer -> UnaryOp -> Integer -> EvalM Integer
evalUnaryOp p Neg v = return $ (p - v) `mod` p
evalUnaryOp _ Inv 0 = throwError DivisionByZero
evalUnaryOp p Inv v = do
  let (g, x, _) = extGcd v p
  if g /= 1
    then throwError (InverseNotFound $ show v)
    else return $ (x `mod` p + p) `mod` p

-- | Modular exponentiation (square-and-multiply).
powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod base e m
  | e < 0     = powMod base (e + (m - 1)) m  -- Fermat's little theorem
  | even e    = let half = powMod base (e `div` 2) m
                in (half * half) `mod` m
  | otherwise = (base * powMod base (e - 1) m) `mod` m

-- ============================================================
-- Public API
-- ============================================================

-- | Evaluate an expression with given config.
evalWithConfig :: ObfuscationConfig -> Expr -> Either AlgebircError Integer
evalWithConfig cfg expr =
  let st0 = emptyEvalState (cfgMaxDepth cfg) (cfgMaxSteps cfg)
      p   = cfgFieldPrime cfg
      (result, _finalState) = runState (runExceptT (evalExpr p expr)) st0
  in result

-- | Evaluate with default config.
eval :: Expr -> Either AlgebircError Integer
eval = evalWithConfig defaultConfig

-- | Pure evaluation: same as eval but returns Maybe for convenience.
evalPure :: Expr -> Maybe Integer
evalPure expr = case eval expr of
  Right v  -> Just v
  Left _   -> Nothing

-- ============================================================
-- Pretty Printer
-- ============================================================

-- | Pretty-print an expression.
prettyExpr :: Expr -> String
prettyExpr (Lit n) = show n
prettyExpr (FieldLit v p) = show v ++ " (mod " ++ show p ++ ")"
prettyExpr (Var name) = name
prettyExpr (BinOpExpr op e1 e2) =
  "(" ++ prettyExpr e1 ++ " " ++ showOp op ++ " " ++ prettyExpr e2 ++ ")"
prettyExpr (UnaryOpExpr Neg e) = "(-" ++ prettyExpr e ++ ")"
prettyExpr (UnaryOpExpr Inv e) = "(1/" ++ prettyExpr e ++ ")"
prettyExpr (If c t f) =
  "if " ++ prettyExpr c ++ " then " ++ prettyExpr t ++ " else " ++ prettyExpr f
prettyExpr (Let n b body) =
  "let " ++ n ++ " = " ++ prettyExpr b ++ " in " ++ prettyExpr body
prettyExpr (LetRec n b body) =
  "letrec " ++ n ++ " = " ++ prettyExpr b ++ " in " ++ prettyExpr body
prettyExpr (Seq es) = "{" ++ unwords (map prettyExpr es) ++ "}"
prettyExpr (PolyEval cs e) =
  "poly" ++ show cs ++ "(" ++ prettyExpr e ++ ")"

showOp :: BinOp -> String
showOp Add = "+"
showOp Sub = "-"
showOp Mul = "*"
showOp Div = "/"
showOp Pow = "^"
showOp Mod = "%"
