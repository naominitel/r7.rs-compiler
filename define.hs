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
    codegen lbd st = return ([], st)