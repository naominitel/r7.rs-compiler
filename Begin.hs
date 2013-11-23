module Begin
(
    Begin(Begin)
) where

import AST
import Bytecode
import Compiler
import Lexer

data Begin = Begin [AST]

instance Show Begin where
    show (Begin asts) = "(begin " ++ show asts ++ ")"

instance Expression Begin where
    codegen (Begin ((AST a) : [])) st p = codegen a st p
    codegen (Begin ((AST a) : rest)) (e, l) pos =
        let ret = codegen a (e, l) False in
        continue ret
            (\st -> codegen (Begin rest) st pos)
            (\iret ir -> iret ++ [(Pop)] ++ ir)
