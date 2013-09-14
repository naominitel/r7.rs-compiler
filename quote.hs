module Quote
(
    Quote(Quote)
) where

import AST
import Compiler
import Lexer
import Tree

-- A quoted expression (quote expr)

data Quote = Quote TokenTree Pos

instance Show Quote where
    show (Quote s _) = show s
 
instance Expression Quote where
    codegen lbd st = return ([], st)
    