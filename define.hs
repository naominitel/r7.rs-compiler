module Define
(
    Define(Define)
) where

import AST
import Bytecode
import Compiler
import Lexer

-- an identifier definition (define id val)  

data Define = Define String AST Pos

instance Show Define where
    show (Define var expr _) = "(define " ++ var ++ " " ++ show expr ++ ")"
 
instance Expression Define where
    codegen (Define i (AST expr) _) st = 
        let rec = codegen expr st
        in rec >>= \(e,st1) -> 
            return ([Alloc 1] ++ e ++ [Store 0] ++ [Push TUnit], 
                envExtend [i] st1)