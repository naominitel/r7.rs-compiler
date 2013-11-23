module Quote
(
    Quote(Quote)
) where

import AST
import Bytecode
import Compiler
import Datum
import Error
import Lexer
import Tree

-- A quoted expression (quote expr)

data Quote = Quote Datum Pos

compileDatum :: Datum -> State -> Result
compileDatum (SimpleDatum constant) st = codegen constant st False

instance Show Quote where
    show (Quote s _) = show s
 
instance Expression Quote where
    codegen (Quote dat _) st _ = compileDatum dat st
    
