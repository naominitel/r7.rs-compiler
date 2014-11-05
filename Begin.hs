module Begin
(
    Begin(Begin)
) where

import AST
import Bytecode
import Compiler
import Result

data Begin = Begin [Expression]

instance Show Begin where
    show (Begin asts) = "(begin " ++ show asts ++ ")"

instance Expand Begin where
    expand ctxt (Begin exprs) =
        Begin $ map (expand ctxt) exprs

instance CompileExpr Begin where
    codegen (Begin []) st _ = Pass [] st
    codegen (Begin [Expr a]) st p = codegen a st p
    codegen (Begin (Expr a : rest)) (e, l) pos = do
        ret <- codegen a (e, l) False
        ret <- Pass [Pop] ret
        codegen (Begin rest) ret pos
