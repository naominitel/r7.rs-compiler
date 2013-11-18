module Set
(
    Set(Set)
) where

import AST
import Bytecode
import Compiler
import Data.Word
import Lexer

data Set = Set String AST Pos

instance Show Set where
    show (Set var expr _) = "(set! " ++ var ++ " " ++ show expr ++ ")"

instance Expression Set where
    codegen (Set i (AST expr) p) st@(env, lbl) _ =
        case envFetch i env of
            (Just envId) ->
                let rec = codegen expr (env, lbl) False
                in rec >>= \(e, st1) ->
                    return (e ++ [Store (fromIntegral envId :: Word64)]
                        ++ [Push TUnit], st1)
            (Nothing) ->
                fail $ "Unbound variable " ++ i ++ " at " ++ show p
