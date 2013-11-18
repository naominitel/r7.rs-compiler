module Constant
(
    Constant(IntConstant, BoolConstant, Symbol)
) where

import Data.Word

import AST
import Bytecode
import Compiler
import Lexer

-- This module handles literal constants

data Constant 
    = IntConstant Word64 Pos
    | BoolConstant Bool Pos
    | Symbol String Pos

instance Show Constant where
    show (IntConstant i _) = show i
    show (BoolConstant b _) = show b
    show (Symbol s _) = show s
 
instance Expression Constant where
    codegen (IntConstant i _) st _ = return ([Push $ TInt i], st)
    codegen (BoolConstant b _) st _ = return ([Push $ TBool b], st)
    codegen (Symbol s _) st _ = return ([Push $ TSym s], st)
