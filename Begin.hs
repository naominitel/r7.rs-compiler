module Begin
(
    Begin(Begin)
) where

import AST
import Bytecode
import Compiler
import Lexer

data Begin = Begin [Expression]

instance Show Begin where
    show (Begin asts) = "(begin " ++ show asts ++ ")"

instance Expand Begin where
    expand ctxt (Begin exprs) =
        Begin $ map (expand ctxt) exprs

instance CompileExpr Begin where
    codegen (Begin ((Expr a) : [])) st p = codegen a st p
    codegen (Begin ((Expr a) : rest)) (e, l) pos =
        let ret = codegen a (e, l) False in
        continue ret
            (\st -> codegen (Begin rest) st pos)
            (\iret ir -> iret ++ [(Pop)] ++ ir)
