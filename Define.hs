module Define
(
    Define(Define),
    Scope(Scope),
    compileScope,
    scopeAddDef,
    scopeAddExpr
) where

import AST
import Begin
import Bytecode
import Compiler
import Error
import Lexer
import Data.Word

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

compileDefines :: [Define] -> Word64 -> State -> Result
compileDefines [] _ st = Pass [] st
compileDefines ((Define var (Expr e) _) : r) i st =
    let rec = codegen e st False in
    continue rec
            (compileDefines r $ i + 1)
            (\irec ir -> irec ++ [(Store i)] ++ ir)

instance Expand Scope where
    expand c (Scope defs body) = Scope (map (expand c) defs) (expand c body)

compileScope :: Scope -> State -> Result
compileScope (Scope defs body) st =
    let envsize = fromIntegral (length defs) :: Word64
        env = map (\(Define var _ _) -> var) defs
        st1 = envExtend env st
        rec = compileDefines defs 0 st1
    in continue rec
        (\st -> codegen body st True)
        (\idefs ibody -> [Alloc envsize] ++ idefs ++ ibody)
