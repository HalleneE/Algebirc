-- |
-- Module      : Algebirc.Obfuscation.AST
-- Description : Haskell-like expression AST for obfuscation targets
-- License     : MIT
--
-- This module defines the source-level AST that represents
-- the programs being obfuscated. It is intentionally simpler than
-- full Haskell to keep the obfuscation tractable.

module Algebirc.Obfuscation.AST
  ( -- * Source AST
    SourceExpr(..)
  , SourceDecl(..)
  , SourceModule(..)
    -- * Serialization
  , serializeModule
  , deserializeModule
    -- * Utilities
  , exprSize
  , exprDepth
  , collectVars
  , prettySource
  ) where

import Data.List (intercalate, nub)

-- ============================================================
-- Source-Level AST
-- ============================================================

-- | A source module: a collection of declarations.
data SourceModule = SourceModule
  { smName  :: !String
  , smDecls :: ![SourceDecl]
  } deriving (Show, Eq)

-- | A top-level declaration.
data SourceDecl
  = FuncDecl !String ![String] !SourceExpr   -- ^ function name, params, body
  | ValDecl  !String !SourceExpr              -- ^ value binding
  deriving (Show, Eq)

-- | Source expression — a simplified Haskell-like language.
data SourceExpr
  = SLit  !Integer                              -- ^ integer literal
  | SVar  !String                               -- ^ variable
  | SApp  !SourceExpr !SourceExpr               -- ^ application
  | SLam  !String !SourceExpr                   -- ^ lambda: \x -> body
  | SLet  !String !SourceExpr !SourceExpr       -- ^ let x = e1 in e2
  | SIf   !SourceExpr !SourceExpr !SourceExpr   -- ^ if-then-else
  | SBinOp !String !SourceExpr !SourceExpr      -- ^ binary operator
  | SList ![SourceExpr]                         -- ^ list literal
  | STuple ![SourceExpr]                        -- ^ tuple
  | SBlock ![SourceExpr]                        -- ^ sequence of expressions
  deriving (Show, Eq)

-- ============================================================
-- Metrics
-- ============================================================

-- | Count AST nodes.
exprSize :: SourceExpr -> Int
exprSize (SLit _)       = 1
exprSize (SVar _)       = 1
exprSize (SApp f a)     = 1 + exprSize f + exprSize a
exprSize (SLam _ b)     = 1 + exprSize b
exprSize (SLet _ e1 e2) = 1 + exprSize e1 + exprSize e2
exprSize (SIf c t f)    = 1 + exprSize c + exprSize t + exprSize f
exprSize (SBinOp _ l r) = 1 + exprSize l + exprSize r
exprSize (SList xs)     = 1 + sum (map exprSize xs)
exprSize (STuple xs)    = 1 + sum (map exprSize xs)
exprSize (SBlock xs)    = 1 + sum (map exprSize xs)

-- | Maximum nesting depth.
exprDepth :: SourceExpr -> Int
exprDepth (SLit _)       = 0
exprDepth (SVar _)       = 0
exprDepth (SApp f a)     = 1 + max (exprDepth f) (exprDepth a)
exprDepth (SLam _ b)     = 1 + exprDepth b
exprDepth (SLet _ e1 e2) = 1 + max (exprDepth e1) (exprDepth e2)
exprDepth (SIf c t f)    = 1 + maximum [exprDepth c, exprDepth t, exprDepth f]
exprDepth (SBinOp _ l r) = 1 + max (exprDepth l) (exprDepth r)
exprDepth (SList xs)     = if null xs then 0 else 1 + maximum (map exprDepth xs)
exprDepth (STuple xs)    = if null xs then 0 else 1 + maximum (map exprDepth xs)
exprDepth (SBlock xs)    = if null xs then 0 else 1 + maximum (map exprDepth xs)

-- | Collect all free variable names.
collectVars :: SourceExpr -> [String]
collectVars = nub . go
  where
    go (SLit _)       = []
    go (SVar v)       = [v]
    go (SApp f a)     = go f ++ go a
    go (SLam _ b)     = go b
    go (SLet _ e1 e2) = go e1 ++ go e2
    go (SIf c t f)    = go c ++ go t ++ go f
    go (SBinOp _ l r) = go l ++ go r
    go (SList xs)     = concatMap go xs
    go (STuple xs)    = concatMap go xs
    go (SBlock xs)    = concatMap go xs

-- ============================================================
-- Serialization (simple S-expression format)
-- ============================================================

-- | Serialize a module to a string representation.
serializeModule :: SourceModule -> String
serializeModule (SourceModule name decls) =
  "(module " ++ name ++ "\n" ++
  unlines (map serializeDecl decls) ++
  ")"

serializeDecl :: SourceDecl -> String
serializeDecl (FuncDecl name params body) =
  "  (defn " ++ name ++ " (" ++ unwords params ++ ") " ++ serializeExpr body ++ ")"
serializeDecl (ValDecl name body) =
  "  (val " ++ name ++ " " ++ serializeExpr body ++ ")"

serializeExpr :: SourceExpr -> String
serializeExpr (SLit n)       = show n
serializeExpr (SVar v)       = v
serializeExpr (SApp f a)     = "(app " ++ serializeExpr f ++ " " ++ serializeExpr a ++ ")"
serializeExpr (SLam p b)     = "(lam " ++ p ++ " " ++ serializeExpr b ++ ")"
serializeExpr (SLet v e1 e2) = "(let " ++ v ++ " " ++ serializeExpr e1 ++ " " ++ serializeExpr e2 ++ ")"
serializeExpr (SIf c t f)    = "(if " ++ serializeExpr c ++ " " ++ serializeExpr t ++ " " ++ serializeExpr f ++ ")"
serializeExpr (SBinOp op l r) = "(" ++ op ++ " " ++ serializeExpr l ++ " " ++ serializeExpr r ++ ")"
serializeExpr (SList xs)     = "(list " ++ unwords (map serializeExpr xs) ++ ")"
serializeExpr (STuple xs)    = "(tuple " ++ unwords (map serializeExpr xs) ++ ")"
serializeExpr (SBlock xs)    = "(block " ++ unwords (map serializeExpr xs) ++ ")"

-- | Placeholder deserializer (full parser in future iteration).
deserializeModule :: String -> Either String SourceModule
deserializeModule _ = Left "Parser not yet implemented"

-- ============================================================
-- Pretty Printing
-- ============================================================

prettySource :: SourceExpr -> String
prettySource (SLit n)       = show n
prettySource (SVar v)       = v
prettySource (SApp f a)     = prettySource f ++ " " ++ parens (prettySource a)
prettySource (SLam p b)     = "\\" ++ p ++ " -> " ++ prettySource b
prettySource (SLet v e1 e2) = "let " ++ v ++ " = " ++ prettySource e1 ++ " in " ++ prettySource e2
prettySource (SIf c t f)    = "if " ++ prettySource c ++ " then " ++ prettySource t ++ " else " ++ prettySource f
prettySource (SBinOp op l r) = parens (prettySource l ++ " " ++ op ++ " " ++ prettySource r)
prettySource (SList xs)     = "[" ++ intercalate ", " (map prettySource xs) ++ "]"
prettySource (STuple xs)    = "(" ++ intercalate ", " (map prettySource xs) ++ ")"
prettySource (SBlock xs)    = "{ " ++ intercalate "; " (map prettySource xs) ++ " }"

parens :: String -> String
parens s = "(" ++ s ++ ")"
