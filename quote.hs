module Quote
(
    Quote(Quote)
) where

import AST
import Bytecode
import Compiler
import Datum
import Lexer
import Tree

-- A quoted expression (quote expr)

data Quote = Quote Datum Pos

compileDatum :: Datum -> CompilerState -> Either String ([Instr], CompilerState)
compileDatum (SimpleDatum constant) st = codegen constant st

instance Show Quote where
    show (Quote s _) = show s
 
instance Expression Quote where
    codegen (Quote dat _) st = compileDatum dat st
    
