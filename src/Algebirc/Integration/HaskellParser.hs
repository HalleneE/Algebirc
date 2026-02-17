-- |
-- Module      : Algebirc.Integration.HaskellParser
-- Description : Bridge antara haskell-src-exts dan internal AST
-- License     : MIT
--
-- Parse file .hs asli menggunakan haskell-src-exts, lalu convert ke
-- internal SourceModule. Juga bisa generate kembali Haskell source
-- dari SourceModule.

module Algebirc.Integration.HaskellParser
  ( -- * Parsing
    parseHaskellFile
  , parseHaskellSource
    -- * Code Generation
  , sourceToHaskell
    -- * Metadata
  , ParseResult(..)
  ) where

import Algebirc.Obfuscation.AST

import qualified Language.Haskell.Exts as HSE
import qualified Language.Haskell.Exts.SrcLoc as Loc

-- ============================================================
-- Types
-- ============================================================

-- | Hasil parsing beserta metadata.
data ParseResult = ParseResult
  { prModule    :: !SourceModule   -- ^ Internal AST
  , prOriginal  :: !String         -- ^ Source code asli
  , prLineCount :: !Int            -- ^ Jumlah baris
  } deriving (Show, Eq)

-- ============================================================
-- Parsing
-- ============================================================

-- | Parse file .hs dari disk.
parseHaskellFile :: FilePath -> IO (Either String ParseResult)
parseHaskellFile path = do
  src <- readFile path
  return $ parseHaskellSource src

-- | Parse Haskell source string.
parseHaskellSource :: String -> Either String ParseResult
parseHaskellSource src =
  let mode = HSE.defaultParseMode
               { HSE.extensions = defaultExtensions
               , HSE.fixities   = Just HSE.baseFixities
               }
  in case HSE.parseModuleWithMode mode src of
       HSE.ParseOk hseModule ->
         let sm = hseToSource hseModule
             lc = length (lines src)
         in Right ParseResult
              { prModule    = sm
              , prOriginal  = src
              , prLineCount = lc
              }
       HSE.ParseFailed loc msg ->
         Left $ "Parse error at " ++ show (Loc.srcLine loc)
             ++ ":" ++ show (Loc.srcColumn loc)
             ++ ": " ++ msg

-- | Ekstensi GHC yang umum dipakai.
defaultExtensions :: [HSE.Extension]
defaultExtensions =
  [ HSE.EnableExtension HSE.ScopedTypeVariables
  , HSE.EnableExtension HSE.DeriveGeneric
  , HSE.EnableExtension HSE.DeriveAnyClass
  , HSE.EnableExtension HSE.OverloadedStrings
  , HSE.EnableExtension HSE.MultiParamTypeClasses
  , HSE.EnableExtension HSE.FlexibleContexts
  , HSE.EnableExtension HSE.FlexibleInstances
  , HSE.EnableExtension HSE.TypeFamilies
  , HSE.EnableExtension HSE.GADTs
  , HSE.EnableExtension HSE.LambdaCase
  , HSE.EnableExtension HSE.TupleSections
  , HSE.EnableExtension HSE.BangPatterns
  ]

-- ============================================================
-- HSE AST → Internal SourceModule
-- ============================================================

-- | Convert HSE module ke internal SourceModule.
hseToSource :: HSE.Module HSE.SrcSpanInfo -> SourceModule
hseToSource (HSE.Module _ mHead _pragmas _imports decls) =
  let name = case mHead of
               Just (HSE.ModuleHead _ (HSE.ModuleName _ n) _ _) -> n
               Nothing -> "Main"
      sourceDecls = concatMap convertDecl decls
  in SourceModule name sourceDecls
hseToSource (HSE.XmlPage _ _ _ _ _ _ _) =
  SourceModule "XmlPage" []
hseToSource (HSE.XmlHybrid _ _ _ _ _ _ _ _ _) =
  SourceModule "XmlHybrid" []

-- | Convert satu top-level declaration.
convertDecl :: HSE.Decl HSE.SrcSpanInfo -> [SourceDecl]
convertDecl (HSE.FunBind _ matches) =
  map convertMatch matches
convertDecl (HSE.PatBind _ pat rhs _mBinds) =
  let name = patToName pat
      body = rhsToExpr rhs
  in [ValDecl name body]
convertDecl (HSE.TypeSig _ names typ) =
  -- Type signature → simpan sebagai metadata dalam ValDecl
  let nameStrs = map nameToStr names
      typStr = HSE.prettyPrint typ
  in [ValDecl ("_typesig_" ++ head nameStrs)
              (SVar $ unwords nameStrs ++ " :: " ++ typStr)]
convertDecl (HSE.DataDecl _ _ _ctx dHead cons _derivs) =
  let name = declHeadToName dHead
      conStrs = map conToStr cons
  in [ValDecl ("_data_" ++ name)
              (SList (map SVar conStrs))]
convertDecl (HSE.TypeDecl _ dHead typ) =
  let name = declHeadToName dHead
  in [ValDecl ("_type_" ++ name) (SVar (HSE.prettyPrint typ))]
convertDecl (HSE.ClassDecl _ _ctx dHead _fundeps _classDecls) =
  let name = declHeadToName dHead
  in [ValDecl ("_class_" ++ name) (SVar "class")]
convertDecl (HSE.InstDecl _ _overlap instRule _instDecls) =
  [ValDecl "_instance" (SVar (HSE.prettyPrint instRule))]
convertDecl other =
  -- Graceful fallback: simpan sebagai pretty-printed string
  [ValDecl "_unsupported" (SVar (HSE.prettyPrint other))]

-- | Convert function match ke FuncDecl.
convertMatch :: HSE.Match HSE.SrcSpanInfo -> SourceDecl
convertMatch (HSE.Match _ name pats rhs _mBinds) =
  let fname   = nameToStr name
      params  = map patToName pats
      body    = rhsToExpr rhs
  in FuncDecl fname params body
convertMatch (HSE.InfixMatch _ pat name pats rhs _mBinds) =
  let fname   = nameToStr name
      params  = map patToName (pat : pats)
      body    = rhsToExpr rhs
  in FuncDecl fname params body

-- ============================================================
-- Expression Conversion
-- ============================================================

-- | Convert HSE Rhs ke SourceExpr.
rhsToExpr :: HSE.Rhs HSE.SrcSpanInfo -> SourceExpr
rhsToExpr (HSE.UnGuardedRhs _ expr) = convertExpr expr
rhsToExpr (HSE.GuardedRhss _ guards) =
  SBlock (map guardToExpr guards)

guardToExpr :: HSE.GuardedRhs HSE.SrcSpanInfo -> SourceExpr
guardToExpr (HSE.GuardedRhs _ stmts expr) =
  SIf (SBlock (map stmtToExpr stmts)) (convertExpr expr) (SVar "otherwise")

stmtToExpr :: HSE.Stmt HSE.SrcSpanInfo -> SourceExpr
stmtToExpr (HSE.Qualifier _ expr) = convertExpr expr
stmtToExpr (HSE.Generator _ pat expr) =
  SBinOp "<-" (SVar (patToName pat)) (convertExpr expr)
stmtToExpr (HSE.LetStmt _ _binds) =
  SVar "let_bindings"
stmtToExpr other = SVar (HSE.prettyPrint other)

-- | Convert HSE expression ke internal SourceExpr.
convertExpr :: HSE.Exp HSE.SrcSpanInfo -> SourceExpr
-- Literals
convertExpr (HSE.Lit _ lit) = convertLit lit
-- Variables
convertExpr (HSE.Var _ qname) = SVar (qnameToStr qname)
convertExpr (HSE.Con _ qname) = SVar (qnameToStr qname)
-- Application
convertExpr (HSE.App _ f a) = SApp (convertExpr f) (convertExpr a)
-- Infix application
convertExpr (HSE.InfixApp _ l (HSE.QVarOp _ op) r) =
  SBinOp (qnameToStr op) (convertExpr l) (convertExpr r)
convertExpr (HSE.InfixApp _ l (HSE.QConOp _ op) r) =
  SBinOp (qnameToStr op) (convertExpr l) (convertExpr r)
-- Lambda
convertExpr (HSE.Lambda _ pats body) =
  foldr (\p acc -> SLam (patToName p) acc) (convertExpr body) pats
-- Let
convertExpr (HSE.Let _ binds body) =
  let bindPairs = convertBinds binds
  in foldr (\(n, e) acc -> SLet n e acc) (convertExpr body) bindPairs
-- If
convertExpr (HSE.If _ cond thenE elseE) =
  SIf (convertExpr cond) (convertExpr thenE) (convertExpr elseE)
-- Case → convert ke nested if
convertExpr (HSE.Case _ scrut alts) =
  let scrutE = convertExpr scrut
  in foldr (\alt acc -> altToIf scrutE alt acc) (SVar "_nomatch") alts
-- Tuple
convertExpr (HSE.Tuple _ _ exprs) =
  STuple (map convertExpr exprs)
-- List
convertExpr (HSE.List _ exprs) =
  SList (map convertExpr exprs)
-- Parenthesized
convertExpr (HSE.Paren _ expr) = convertExpr expr
-- Negation
convertExpr (HSE.NegApp _ expr) =
  SBinOp "-" (SLit 0) (convertExpr expr)
-- Do notation → convert ke block
convertExpr (HSE.Do _ stmts) =
  SBlock (map stmtToExpr stmts)
-- String literal
-- Note: HSE.Lit already handled above (includes String)
-- Type annotation
convertExpr (HSE.ExpTypeSig _ expr _typ) = convertExpr expr
-- Section
convertExpr (HSE.LeftSection _ expr (HSE.QVarOp _ op)) =
  SLam "_x" (SBinOp (qnameToStr op) (convertExpr expr) (SVar "_x"))
convertExpr (HSE.RightSection _ (HSE.QVarOp _ op) expr) =
  SLam "_x" (SBinOp (qnameToStr op) (SVar "_x") (convertExpr expr))
-- Enum
convertExpr (HSE.EnumFrom _ from) =
  SApp (SVar "enumFrom") (convertExpr from)
convertExpr (HSE.EnumFromTo _ from to) =
  SApp (SApp (SVar "enumFromTo") (convertExpr from)) (convertExpr to)
-- Fallback: pretty-print apapun yang belum dihandle
convertExpr other = SVar (HSE.prettyPrint other)

-- | Convert HSE literal.
convertLit :: HSE.Literal HSE.SrcSpanInfo -> SourceExpr
convertLit (HSE.Int _ n _)    = SLit n
convertLit (HSE.Char _ c _)   = SLit (fromIntegral (fromEnum c))
convertLit (HSE.String _ s _) = SVar ("\"" ++ s ++ "\"")
convertLit other              = SVar (HSE.prettyPrint other)

-- | Convert case alternative ke if-then-else chain.
altToIf :: SourceExpr -> HSE.Alt HSE.SrcSpanInfo -> SourceExpr -> SourceExpr
altToIf scrut (HSE.Alt _ pat rhs _mBinds) fallback =
  SIf (SBinOp "==" scrut (SVar (patToName pat)))
      (rhsToExpr rhs)
      fallback

-- | Convert let bindings.
convertBinds :: HSE.Binds HSE.SrcSpanInfo -> [(String, SourceExpr)]
convertBinds (HSE.BDecls _ decls) =
  [ (n, body)
  | d <- decls
  , let ds = convertDecl d
  , FuncDecl n _ body <- ds
  ] ++
  [ (n, body)
  | d <- decls
  , let ds = convertDecl d
  , ValDecl n body <- ds
  ]
convertBinds (HSE.IPBinds _ _) = []

-- ============================================================
-- Helpers: naam extractie
-- ============================================================

nameToStr :: HSE.Name HSE.SrcSpanInfo -> String
nameToStr (HSE.Ident _ s) = s
nameToStr (HSE.Symbol _ s) = "(" ++ s ++ ")"

qnameToStr :: HSE.QName HSE.SrcSpanInfo -> String
qnameToStr (HSE.Qual _ (HSE.ModuleName _ m) name) = m ++ "." ++ nameToStr name
qnameToStr (HSE.UnQual _ name) = nameToStr name
qnameToStr (HSE.Special _ sc) = specialToStr sc

specialToStr :: HSE.SpecialCon HSE.SrcSpanInfo -> String
specialToStr (HSE.UnitCon _)     = "()"
specialToStr (HSE.ListCon _)     = "[]"
specialToStr (HSE.FunCon _)      = "->"
specialToStr (HSE.TupleCon _ _ n) = "(" ++ replicate (n-1) ',' ++ ")"
specialToStr (HSE.Cons _)        = ":"
specialToStr _                   = "_special"

patToName :: HSE.Pat HSE.SrcSpanInfo -> String
patToName (HSE.PVar _ name)       = nameToStr name
patToName (HSE.PLit _ _ lit)      = HSE.prettyPrint lit
patToName (HSE.PTuple _ _ pats)   = "(" ++ concatMap (\p -> patToName p ++ ",") pats ++ ")"
patToName (HSE.PParen _ pat)      = patToName pat
patToName (HSE.PWildCard _)       = "_"
patToName (HSE.PApp _ qn pats)    = qnameToStr qn ++ concatMap (\p -> " " ++ patToName p) pats
patToName (HSE.PInfixApp _ p1 qn p2) = patToName p1 ++ " " ++ qnameToStr qn ++ " " ++ patToName p2
patToName (HSE.PAsPat _ name pat) = nameToStr name ++ "@" ++ patToName pat
patToName (HSE.PList _ pats)      = "[" ++ concatMap (\p -> patToName p ++ ",") pats ++ "]"
patToName (HSE.PatTypeSig _ pat _) = patToName pat
patToName (HSE.PBangPat _ pat)    = "!" ++ patToName pat
patToName other                   = HSE.prettyPrint other

declHeadToName :: HSE.DeclHead HSE.SrcSpanInfo -> String
declHeadToName (HSE.DHead _ name) = nameToStr name
declHeadToName (HSE.DHInfix _ _ name) = nameToStr name
declHeadToName (HSE.DHParen _ dh) = declHeadToName dh
declHeadToName (HSE.DHApp _ dh _) = declHeadToName dh

conToStr :: HSE.QualConDecl HSE.SrcSpanInfo -> String
conToStr (HSE.QualConDecl _ _ _ conDecl) = case conDecl of
  HSE.ConDecl _ name _       -> nameToStr name
  HSE.InfixConDecl _ _ name _ -> nameToStr name
  HSE.RecDecl _ name _       -> nameToStr name

-- ============================================================
-- Code Generation: SourceModule → valid Haskell
-- ============================================================

-- | Generate valid Haskell source dari SourceModule.
-- Output-nya compilable (at least syntax-valid).
sourceToHaskell :: SourceModule -> String
sourceToHaskell (SourceModule name decls) =
  let header = "module " ++ name ++ " where\n\n"
      body = unlines (map declToHaskell decls)
  in header ++ body

declToHaskell :: SourceDecl -> String
declToHaskell (FuncDecl name params body) =
  name ++ " " ++ unwords params ++ " = " ++ exprToHaskell body
declToHaskell (ValDecl name body)
  -- Metadata declarations → restore original form
  | take 9 name == "_typesig_" =
      let realName = drop 9 name
      in case body of
           SVar sig -> sig
           _ -> realName ++ " = " ++ exprToHaskell body
  | take 6 name == "_data_" =
      let typeName = drop 6 name
      in case body of
           SList cons -> "data " ++ typeName ++ " = "
                      ++ unwords (map (\c -> case c of SVar s -> s; _ -> "?") cons)
           _ -> "-- data " ++ typeName
  | take 6 name == "_type_" =
      let typeName = drop 6 name
      in case body of
           SVar t -> "type " ++ typeName ++ " = " ++ t
           _ -> "-- type " ++ typeName
  | take 7 name == "_class_" =
      let className = drop 7 name
      in "-- class " ++ className
  | name == "_instance" =
      case body of
        SVar inst -> "-- instance " ++ inst
        _ -> "-- instance"
  | name == "_unsupported" =
      case body of
        SVar code -> "-- " ++ code
        _ -> "-- unsupported"
  | otherwise = name ++ " = " ++ exprToHaskell body

exprToHaskell :: SourceExpr -> String
exprToHaskell (SLit n)       = show n
exprToHaskell (SVar v)       = v
exprToHaskell (SApp f a)     = exprToHaskell f ++ " " ++ hsParen (exprToHaskell a)
exprToHaskell (SLam p b)     = "\\" ++ p ++ " -> " ++ exprToHaskell b
exprToHaskell (SLet v e1 e2) = "let " ++ v ++ " = " ++ exprToHaskell e1
                             ++ " in " ++ exprToHaskell e2
exprToHaskell (SIf c t f)    = "if " ++ exprToHaskell c
                             ++ " then " ++ exprToHaskell t
                             ++ " else " ++ exprToHaskell f
exprToHaskell (SBinOp op l r) = hsParen (exprToHaskell l ++ " " ++ op ++ " " ++ exprToHaskell r)
exprToHaskell (SList xs)     = "[" ++ commaJoin (map exprToHaskell xs) ++ "]"
exprToHaskell (STuple xs)    = "(" ++ commaJoin (map exprToHaskell xs) ++ ")"
exprToHaskell (SBlock xs)    = "do { " ++ concatMap (\x -> exprToHaskell x ++ "; ") xs ++ "}"

hsParen :: String -> String
hsParen s = "(" ++ s ++ ")"

commaJoin :: [String] -> String
commaJoin [] = ""
commaJoin [x] = x
commaJoin (x:xs) = x ++ ", " ++ commaJoin xs
