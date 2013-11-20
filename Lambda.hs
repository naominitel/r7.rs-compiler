module Lambda
(
    Lambda(Lambda)
) where

import AST
import Bytecode
import Compiler
import Lexer
import Identifier
import Data.Word

-- a lambda-expression, (lambda arg expr)
--   * The [String] is the list of formal arguments
--   * The Maybe String is (optionally), the variadic argument
--   * The AST is the expression

data Lambda = Lambda [String] (Maybe String) AST Pos

instance Show Lambda where
    show (Lambda args _ expr _) = "(lambda " ++ show args ++ " " ++ show expr ++ ")"
 
-- Assembly code generation for Lambda expressions

instance Expression Lambda where
    codegen (Lambda args v (AST expr) p) st _ =
        let (flbl, st1) = nextLabel st
            (clbl, st2@(e, _)) = nextLabel st1
            st3 = case v of
                (Just var) -> envExtend (args ++ [var]) st2
                (Nothing) -> envExtend args st2
            rec = (codegen expr st3 True)
            instrs1 = [Jump clbl, Label flbl]
            instrs2 = [Return, Label clbl,
                Push (TFunc flbl (fromIntegral (length args) ::Word8)
                    (v /= Nothing))]
        in rec >>= \(instrs, (_, pc)) ->
            return (instrs1 ++ instrs ++ instrs2, (e, pc))

