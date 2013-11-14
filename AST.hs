module AST(AST(AST)) where

import Compiler

-- Wrapper type around expression types to be used by compiler functions

data AST = forall a. Expression a => AST a

instance Show AST where
    show (AST a) = show a