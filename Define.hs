module Define
(
    Define(Define),
    Scope(Scope),
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

data Define = Define String AST Pos

instance Show Define where
    show (Define var expr _) = "(define " ++ var ++ " " ++ show expr ++ ")"
 
data Scope = Scope [Define] Begin

scopeAddDef :: Scope -> Define -> Scope
scopeAddDef (Scope l e) d = (Scope (d:l) e)

scopeAddExpr :: Scope -> AST -> Scope
scopeAddExpr (Scope l (Begin e)) expr = Scope l $ Begin (expr:e)

instance Show Scope where
    show (Scope defs expr) = "a scope with defines " ++ show defs ++ "/" ++ show expr

compileDefines :: [Define] -> Word64 -> State -> Result
compileDefines [] _ st = Pass [] st
compileDefines ((Define var (AST e) _) : r) i st =
    let rec = codegen e st False in
    continue rec
            (compileDefines r $ i + 1)
            (\irec ir -> irec ++ [(Store i)] ++ ir)

instance Expression Scope where
    codegen (Scope defs body) st pos =
        let envsize = fromIntegral (length defs) :: Word64
            env = map (\(Define var _ _) -> var) defs
            st1 = envExtend env st
            rec = compileDefines defs 0 st1
        in continue rec
            (\st -> codegen body st pos)
            (\idefs ibody -> [Alloc envsize] ++ idefs ++ ibody)
