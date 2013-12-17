module If
(
    If(If)
) where

import AST
import Bytecode
import Compiler
import Error
import Result

-- a conditional branching (if expr_true expr_false)

data If = If Expression Expression Expression Pos

instance Show If where
    show (If ec et ef _) = "(if " ++ show ec ++ show et ++ show ef ++ ")"

instance Expand If where
    expand c (If ec et ef p) = If (expand c ec) (expand c et) (expand c ef) p
 
instance CompileExpr If where
    codegen (If (Expr ec) (Expr et) (Expr ef) _) st p = do
        let (onfalse, st1) = nextLabel st
        let (contlbl, st2) = nextLabel st1
        cond <- codegen ec st2 False
        lbl  <- Pass [Branch onfalse] cond
        e1   <- codegen et lbl p
        flse <- Pass [Jump contlbl, Label onfalse] e1
        e2   <- codegen ef e1 p
        Pass [Label contlbl] e2
