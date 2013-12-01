module Set
(
    Set(Set)
) where

import AST
import Bytecode
import Compiler
import Data.Word
import Error
import Lexer

data Set = Set String Expression Pos

instance Show Set where
    show (Set var expr _) = "(set! " ++ var ++ " " ++ show expr ++ ")"

instance Expand Set where
    expand ctxt (Set s e p) = Set s (expand ctxt e) p

instance CompileExpr Set where
    codegen (Set i (Expr expr) p) st@(env, lbl) _ =
        case envFetch i env of
            Just envId ->
                let rec = codegen expr st False in
                continue rec
                    (\st -> Pass [] st)
                    (\i _ -> i ++ [Store (fromIntegral envId :: Word64)]
                        ++ [Push TUnit])
            Nothing ->
                Failure [Error ("Unbound variable " ++ i) p] st
