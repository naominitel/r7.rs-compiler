module If
(
    If(If)
) where

import AST
import Bytecode
import Compiler
import Lexer

-- a conditional branching (if expr_true expr_false)

data If = If AST AST AST Pos

instance Show If where
    show (If ec et ef _) = "(if " ++ show ec ++ show et ++ show ef ++ ")"
 
instance Expression If where
    codegen (If (AST ec) (AST et) (AST ef) _) st p =
        let (onfalse, st1) = nextLabel st
            (contlbl, st2) = nextLabel st1
            cond = codegen ec st2 False
        in continue cond
            (\st ->
                let e1 = codegen et st p
                in continue e1
                    (\st -> codegen ef st p)
                    (\t f -> t ++ [(Jump contlbl)] ++ [(Label onfalse)]
                          ++ f ++ [(Label contlbl)]))
            (\cond e -> cond ++ [(Branch onfalse)] ++ e)
