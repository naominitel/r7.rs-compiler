module Lambda
(
    Lambda(Lambda)
) where

import Data.Maybe(isJust)
import AST
import Bytecode
import Compiler
import Define
import Error
import Data.Word
import Result

-- a lambda-expression, (lambda arg expr)
--   * The [String] is the list of formal arguments
--   * The Maybe String is (optionally), the variadic argument
--   * The AST is the expression

data Lambda = Lambda [String] (Maybe String) Scope Pos

instance Show Lambda where
    show (Lambda args _ expr _) = "(lambda " ++ show args ++ " " ++ show expr ++ ")"

-- Assembly code generation for Lambda expressions

instance Expand Lambda where
    expand ctxt (Lambda args var bdy p) = Lambda args var (expand ctxt bdy) p

instance CompileExpr Lambda where
    codegen (Lambda args v expr _) st _ = do
        let (flbl, st1) = nextLabel st
        let (clbl, st2@(e, _)) = nextLabel st1
        let st3 = case v of
                (Just var) -> envExtend (args ++ [var]) st2
                (Nothing) -> envExtend args st2
        start <- Pass [Jump clbl, Label flbl] st3
        mid   <- compileScope expr start
        let end = Pass [Return, Label clbl,
                    Push (TFunc flbl (fromIntegral (length args) :: Word8) $ isJust v)] mid

        -- When returning the new compiler state, be careful to just
        -- return the new label counter, and the old environment
        end >>= \(_, pc) -> return (e, pc)

