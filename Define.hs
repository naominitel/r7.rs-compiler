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

compileDefines :: [Define] -> Word64 -> CompilerState -> Either Error ([Instr], CompilerState)
compileDefines [] _ st = return ([], st)
compileDefines ((Define var (AST e) _) : r) i st =
    case codegen e st False of
        Left err -> Left err
        Right (ins, st) ->
            compileDefines r (i + 1) st >>= \(n, s) ->
            return (ins ++ [(Store i)] ++ n, s)

instance Expression Scope where
    codegen (Scope defs body) st pos =
        let envsize = fromIntegral (length defs) :: Word64
            env = map (\(Define var _ _) -> var) defs
            st1 = envExtend env st
            rec = compileDefines defs 0 st1
        in case rec of
            Left err -> Left err
            Right (d, st2) ->
                codegen body st2 pos >>= \(b, s) ->
                    return ([Alloc envsize] ++ d ++ b, s)
