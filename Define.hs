module Define
(
    Define(Define),
    Scope(Scope),
    compileScope,
    scopeAddDef,
    scopeAddExpr
) where

import Data.Word

import AST
import Begin
import Bytecode
import Compiler
import Error
import Result

-- an identifier definition (define id val)

data Define = Define String Expression Pos

instance Show Define where
    show (Define var expr _) = "(define " ++ var ++ " " ++ show expr ++ ")"

instance Expand Define where
    expand ctxt (Define var e p) = Define var (expand ctxt e) p

data Scope = Scope [Define] Begin

scopeAddDef :: Scope -> Define -> Scope
scopeAddDef (Scope l e) d = (Scope (d:l) e)

scopeAddExpr :: Scope -> Expression -> Scope
scopeAddExpr (Scope l (Begin e)) expr = Scope l $ Begin (expr:e)

instance Show Scope where
    show (Scope defs expr) = "a scope with defines " ++ show defs ++ "/" ++ show expr

compileDefines :: [Define] -> Word64 -> State -> Compiler.Result
compileDefines [] _ st = return st
compileDefines ((Define _ (Expr e) _) : r) i st = do
    rec <- codegen e st False
    rec <- Pass [Store i] rec
    compileDefines r (i + 1) rec

instance Expand Scope where
    expand c (Scope defs body) = Scope (map (expand c) defs) (expand c body)

compileScope :: Scope -> State -> Compiler.Result
compileScope (Scope defs body) st = do
    let envsize = fromIntegral (length defs) :: Word64
    let env = map (\(Define var _ _) -> var) defs
    let st1 = envExtend env st
    alloc <- Pass [Alloc envsize] st1
    defs  <- compileDefines defs 0 alloc
    codegen body defs True
