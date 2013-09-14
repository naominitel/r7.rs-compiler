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
    codegen (If (AST ec) (AST et) (AST ef) _) st =
        let (onfalse, st1) = nextLabel st
            (contlbl, st2) = nextLabel st1
            cond = codegen ec st2
        in cond >>= \(econd, st3) -> 
            let e1 = codegen et st3
            in e1 >>= \(etrue, st4) ->
                let e2 = codegen ef st4 
                in e2 >>= \(efalse, st5) -> return (
                    econd ++ [(Branch onfalse)] ++ 
                    etrue ++ [(Jump contlbl)] ++ [(Label onfalse)] ++
                    efalse ++ [(Label contlbl)], st5)