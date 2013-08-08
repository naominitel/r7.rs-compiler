module AST 
( 
    AST(Constant, Identifier, Quote, Lambda, Define, If, Apply),
    Value(Integer, Boolean, Function, Exception)
) where

-- This module defines basic data structures for representing an abstract
-- syntax tree of the core language. Each data constructor represents a syntaxic
-- construction of the core language.

import Lexer

data Value 
    = Integer Int
    | Function String AST
    | Exception String
    | Boolean Bool

instance Show Value where
    show (Integer i) = show i
    show (Function _ _) = "<*function*>"
    show (Exception s) = "<*exception* : " ++ s ++ " >"

instance Eq Value where
    (Integer i) == (Integer j) = i == j
    (Boolean b) == (Boolean a) = a == b
    _ == _                     = False 

-- Basic data structure for core langugage syntax
-- Any element as its position in the file attached

data AST

-- A numeric, string or boolean constant
    = Constant Value Pos

-- An identifier, such as "var"
    | Identifier String Pos

-- A quoted expression (quote expr)
    | Quote String Pos

-- a lambda-expression, (lambda arg expr)
    | Lambda String AST Pos

-- a procedure is an evaluator-builtin function
    | Procedure Token Pos

-- an identifier definition (define id val)  
    | Define String AST Pos

-- a conditional branching (if expr_true expr_false)
    | If AST AST AST Pos

-- a function call (f arg)
    | Apply AST AST Pos 

instance Show AST where
    show (Constant t _)   = show t
    show (Identifier t _) = "ID: " ++ t
    show (Quote t _)      = "(quote " ++ t ++ ")"
    show (Lambda a e _)   = "(lambda (" ++ a ++ ") " ++ show e ++ ")"
    show (Define i e _)   = "(define " ++ i ++ " " ++ show e ++ ")"
    show (If eif e1 e2 _) = "(if " ++ show eif ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Apply f a _)    = "(" ++ show f ++ " " ++ show a ++ ")"
