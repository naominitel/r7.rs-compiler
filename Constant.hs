module Constant
(
    Constant(IntConstant, BoolConstant, Symbol)
) where

import Data.Word

import AST
import Bytecode
import Compiler
import Lexer
import Result

-- This module handles literal constants

data Constant 
    = IntConstant Word64 Pos
    | BoolConstant Bool Pos
    | Symbol String Pos

instance Show Constant where
    show (IntConstant i _)  = show i
    show (BoolConstant b _) = show b
    show (Symbol s _)       = show s

instance Expand Constant where
    expand ctx c = c
 
instance CompileExpr Constant where
    codegen (IntConstant i _) st _  = Pass [Push $ TInt i] st
    codegen (BoolConstant b _) st _ = Pass [Push $ TBool b] st
    codegen (Symbol s _) st _       = Pass [Push $ TSym s] st
