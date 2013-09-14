module Constant
(
    Constant(IntConstant, BoolConstant)
) where

import Data.Word

import AST
import Bytecode
import Compiler
import Lexer

-- This module handles literal constants

data Constant 
    = IntConstant Word32 Pos
    | BoolConstant Bool Pos

instance Show Constant where
    show (IntConstant i _) = show i
    show (BoolConstant b _) = show b
 
instance Expression Constant where
    codegen (IntConstant i _) st = return ([Push $ TInt i], st)
    codegen (BoolConstant b _) st = return ([Push $ TBool b], st)
    