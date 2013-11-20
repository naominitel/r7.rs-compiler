-- parser.hs
-- Copyright (c) Naomi Nitel <naominitel@gmail.com>

module Parser 
( 
    arborize, 
    expand, 
    parser 
) where

import Control.Applicative
import Control.Monad.Instances

import Apply
import AST
import Begin
import Bytecode
import Compiler
import Constant
import Datum
import Define
import Identifier
import If
import Lambda
import Lexer
import Quote
import Set
import Syntax
import Tree

-- The parser first transforms the flat token list into a tree structure.
-- Braces are removed, and any other token is a leaf or node of the tree.
-- Any token has attached its contents and position in file

arborize :: [Token] -> ([TokenTree], [Token])
arborize [] = ([], [])
arborize (TokClose _:s) = ([], s)
arborize (TokOpen _:s) = 
    let (rec, cont) = arborize s
        (next, end) = arborize cont
    in (TokNode rec : next, end)
arborize (tok : s) =
    let (rec, cont) = arborize s
    in (TokLeaf tok : rec, cont)

---- The parser now expand the tree into a typed data structures, replacing
---- plain string lists by syntaxic elements of the core langage and 
---- expand them when necessary. 
---- Expanding rules are detailed in doc/expanding.

---- The data structures used here are defined in the AST module

---- curryfy a function call
---- takes a lambda-expr and a list of args and return the curryfied form
---- (Apply ... (Apply f e1) ... en)
---- Arguments are passed in reverse order

--curryfiedCall :: AST -> [Either String AST] -> Pos -> Either String AST
--curryfiedCall lambda [] pos             = return lambda
--curryfiedCall lambda (Right arg:[]) pos = return $ Apply lambda arg pos
--curryfiedCall lambda (Right arg:rs) pos = curryfiedCall lambda rs pos >>= \a -> return $ Apply a arg pos
--curryfiedCall _ (Left s:_) _            = fail s

---- curryfy a lambda expression
---- (lambda (a b c) e) -> (lambda (a) (lambda (b) (lambda (c) e)))
---- takes the argument list and the return expr of the lambda expression
---- a lambda with no params is a constant : (lambda () e) -> e

--curryfiedLambda :: [String] -> AST -> Pos -> AST
--curryfiedLambda [] expr _         = expr
--curryfiedLambda (arg:[]) expr p   = (Lambda arg expr p)
--curryfiedLambda (arg:args) expr p = (Lambda arg (curryfiedLambda args expr p) p)

-- expand: core of the parser. expands a TokenTree into an AST, which is
-- basically the same thing with token sequences assembled into semantic objects
-- Some langage constructs are translated to others.

expand :: TokenTree -> Either String AST

-- Parsing for (let ((e1 v1) ... (en vn)) expr) -> ((lambda (e1 ... en) expr) v1 ... vn)

expand (TokNode (TokLeaf (TokLet p) : (TokNode bindings) : expr : [])) =
    let args   = letArgs bindings
        ids    = args >>= letArgsIdentifiers
        vals   = args >>= letArgsValues
        lambda = ids >>= \i -> return $ TokNode $ TokLeaf (TokLambda p) : (TokNode $ map TokLeaf i) : expr : []
        apply  = (vals >>= \v -> return $ \l -> TokNode (l:v)) <*> lambda
    in apply >>= expand

expand (TokNode (TokLeaf (TokLet p) : _)) =
    fail $ "Malformed let-expr at " ++ show p

-- Parsing for (lambda (e1 ... en) expr)

expand (TokNode (TokLeaf (TokLambda p) : (TokNode lst) : expr : [])) =
    let args = lambdaArgs lst
        lbda = (args >>= \(a, v) -> return $ Lambda a v)
            <*> expand expr <*> return p
    in lbda >>= return . AST

-- Parsing for (lambda (e1 ... en) expr1 ... exprn) (implicit begin)

expand (TokNode (lbd@(TokLeaf (TokLambda p)) : args@(TokNode lst) : exprs)) = 
    let begin  = TokNode $ (TokLeaf $ (TokBegin p)) : exprs
        lambda = TokNode $ [lbd, args, begin]
    in expand lambda

expand (TokNode (TokLeaf (TokLambda p) : _)) = 
    fail $ "Malformed lambda-expr at " ++ show p

-- Parsing for (set! id val)

expand (TokNode (TokLeaf (TokSet _) : (TokLeaf (TokId i p)) : expr : [])) =
    expand expr >>= \e -> return $ AST $ Set i e p

expand (TokNode (TokLeaf (TokSet p) : _)) =
    fail $ "Syntax error: malformed set!-expression at: " ++ show p

-- Parsing for (define id val)

expand (TokNode (TokLeaf (TokDefine _) : (TokLeaf (TokId i p)) : expr : [])) =
    expand expr >>= \e -> return $ AST $ Define i e p

-- Parsing for (define (id args) val) -> (define id (lambda (args)) val)

expand (TokNode (TokLeaf (TokDefine _) : (TokNode (TokLeaf (TokId i p) : args)) : expr : [])) =
    let lambda = (TokNode (TokLeaf (TokLambda p) : (TokNode args) : expr : []))
        parsedLambda = expand lambda
    in parsedLambda >>= \l -> return $ AST $ Define i l p

expand (TokNode (TokLeaf (TokDefine p) : _)) = 
    fail $ "Malformed define expression at " ++ show p

-- Parsing for (if expr-bool expr-if expr-else)

expand (TokNode (TokLeaf (TokIf p) : exprif : expr1 : expr2 : [])) =
    let parsedif = expand exprif
        parsed1  = expand expr1
        parsed2  = expand expr2
        ifexpr = (parsedif >>= return . If) <*> parsed1 <*> parsed2 <*> return p
    in ifexpr >>= return . AST

expand (TokNode (TokLeaf (TokIf p) : _)) =
    Left $ "Malformed if-expression at " ++ show p

-- Parsing for (quote expr)

expand (TokNode (TokLeaf (TokQuote p) : expr : [])) = 
    return $ AST $ Quote (getDatum expr) p

expand (TokNode (TokLeaf (TokQuote p) : _)) =
    fail $ "Malformed quote-expression at " ++ show p

-- Parsing for (begin expr)

expand (TokNode (TokLeaf (TokBegin p) : exprs)) =
    let parsedExprs = mapM expand exprs
    in parsedExprs >>= \e -> return $ AST $ Begin e p

-- Parsing for function application (f a1 a2 a3)

expand (TokNode (func : args)) = 
    let parsedFunc = expand func
        parsedArgs = mapM expand args 
        expr =  (parsedFunc >>= return . Apply) <*> parsedArgs <*> return (Pos 0 0) -- FIXME
    in expr >>= return . AST

-- Parsing for anything else, like "var" or "1"

expand (TokLeaf (TokBool b p))  = return $ AST $ BoolConstant b p
expand (TokLeaf (TokInt i p))   = return $ AST $ IntConstant i p
expand (TokLeaf (TokId s p)) = return $ AST $ Identifier s p

-- parser: entry point for the parser

parser :: [Token] -> Either String [AST]

parser tokens = 
    case arborize tokens of
        (exprs, []) -> mapM expand exprs -- Fixme: should be map expand exprs
        (_, _) -> fail "Parse error: extra tokens after parenthesis"
