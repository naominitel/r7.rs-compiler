module Lambda
(
    Lambda(Lambda)
) where

import AST
import Bytecode
import Compiler
import Lexer
import Identifier
import Debug.Trace

-- a lambda-expression, (lambda arg expr)

data Lambda = Lambda [String] AST Pos

instance Show Lambda where
    show (Lambda args expr _) = "(lambda " ++ show args ++ " " ++ show expr ++ ")" 
 
-- Assembly code generation for Lambda expressions

instance Expression Lambda where
    codegen (Lambda args (AST expr) p) st =
        let (flbl, st1) = nextLabel st
            (clbl, st2@(e, _)) = nextLabel st1
            st3 = envExtend args st2
            rec = (codegen expr st3)
            instrs1 = [Jump clbl, Label flbl]
            instrs2 = [Return, Label clbl, Push (TFunc flbl)]
        in rec >>= \(instrs, (_, pc)) ->
            return (instrs1 ++ instrs ++ instrs2, (e, pc))

