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
    codegen (Begin ((AST a):[]) _) st = codegen a st
    codegen (Begin ((AST a):rest) p) st = case codegen a st of
        Left err -> Left err
        Right (e, st1@(ev,l)) -> codegen (Begin rest p) st1 >>= \(i, st2) -> 
            return (e ++ [(Pop)] ++ i, st2)