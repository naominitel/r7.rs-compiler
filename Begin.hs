module Begin
(
    Begin(Begin)
) where

import AST
import Bytecode
import Compiler
import Lexer

data Begin = Begin [AST] Pos

instance Show Begin where
    show (Begin asts _) = "(begin " ++ show asts ++ ")"

instance Expression Begin where
    codegen (Begin ((AST a):[]) _) st p = codegen a st p
    codegen (Begin ((AST a):rest) p) (e, l) pos =
        case codegen a (e, l) False of
            Left err -> Left err
            Right (e, st1@(ev, l)) ->
                codegen (Begin rest p) (ev, l) pos >>= \(i, st2) ->
                return (e ++ [(Pop)] ++ i, st2)
