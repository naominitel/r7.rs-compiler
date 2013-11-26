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
import Error
import Identifier
import If
import Lambda
import Lexer
import Library
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

-- The parser now expand the tree into a typed data structures, replacing
-- plain string lists by syntaxic elements of the core langage and
-- expand them when necessary.
-- Expanding rules are detailed in doc/expanding.

-- The data structures used here are defined in the AST module

-- expand: core of the parser. expands a TokenTree into an AST, which is
-- basically the same thing with token sequences assembled into semantic objects
-- Some langage constructs are translated to others.

expand :: TokenTree -> Either Error AST

-- Parsing for (let ((e1 v1) ... (en vn)) expr) -> ((lambda (e1 ... en) expr) v1 ... vn)

expand (TokNode (TokLeaf (TokLet p) : (TokNode bindings) : expr : [])) =
    let args   = letArgs bindings
        ids    = args >>= letArgsIdentifiers
        vals   = args >>= letArgsValues
        lambda = ids >>= \i -> return $ TokNode $
            TokLeaf (TokLambda p) : (TokNode $ map TokLeaf i) : expr : []
        apply  = (vals >>= \v -> return $ \l -> TokNode (l:v)) <*> lambda
    in apply >>= expand

expand (TokNode (TokLeaf (TokLet p) : _)) =
    fail $ "Malformed let-expr at " ++ show p

-- Parsing for (lambda (e1 ... en) expr)

expand (TokNode (TokLeaf (TokLambda p) : (TokNode lst) : exprs)) =
    let args = lambdaArgs lst
        body = (parseBody exprs) >>= return . AST
        lbda = (args >>= \(a, v) -> return $ Lambda a v) <*> body <*> return p
    in lbda >>= return . AST

expand (TokNode (TokLeaf (TokLambda p) : _)) = 
    fail $ "Malformed lambda-expr at " ++ show p

-- Parsing for (set! id val)

expand (TokNode (TokLeaf (TokSet _) : (TokLeaf (TokId i p)) : expr : [])) =
    expand expr >>= \e -> return $ AST $ Set i e p

expand (TokNode (TokLeaf (TokSet p) : _)) =
    fail $ "Syntax error: malformed set!-expression at: " ++ show p

-- Parsing for (if expr-bool expr-if expr-else)

expand (TokNode (TokLeaf (TokIf p) : exprif : expr1 : expr2 : [])) =
    let parsedif = expand exprif
        parsed1  = expand expr1
        parsed2  = expand expr2
        ifexpr = (parsedif >>= return . If) <*> parsed1 <*> parsed2 <*> return p
    in ifexpr >>= return . AST

expand (TokNode (TokLeaf (TokIf p) : _)) =
    Left $ Error "Malformed if-expression" p

-- Parsing for (quote expr)

expand (TokNode (TokLeaf (TokQuote p) : expr : [])) = 
    return $ AST $ Quote (getDatum expr) p

expand (TokNode (TokLeaf (TokQuote p) : _)) =
    Left $ Error "Malformed quote-expression" p

-- Parsing for (begin expr)

expand (TokNode (TokLeaf (TokBegin p) : exprs)) =
    let parsedExprs = mapM expand exprs
    in parsedExprs >>= \e -> return $ AST $ Begin e

-- Parsing for (define-library)

expand (TokNode (TokLeaf (TokDeflib p) : (TokNode n) : stmts)) =
    let bdy = parseBody stmts
        name = parseLibname n
    in (mcons bdy name) >>= \(bdy, name) -> return $ AST $ Library name bdy

expand (TokNode (TokLeaf (TokDeflib p) : _)) =
    Left $ Error "Malformed library definition" p

-- Parsing for function application (f a1 a2 a3)

expand (TokNode (func : args)) = 
    let parsedFunc = expand func
        parsedArgs = mapM expand args 
        expr =  (parsedFunc >>= return . Apply) <*> parsedArgs
    in expr >>= return . AST

expand (TokLeaf (TokDefine _)) = fail $ "Define not allowed in this context"

-- Parsing for anything else, like "var" or "1"

expand (TokLeaf (TokBool b p))  = return $ AST $ BoolConstant b p
expand (TokLeaf (TokInt i p))   = return $ AST $ IntConstant i p
expand (TokLeaf (TokId s p))    = return $ AST $ Identifier s p

mcons :: Either Error a -> Either Error b -> Either Error (a, b)
mcons (Left err) _ = Left err
mcons _ (Left err) = Left err
mcons (Right a) (Right b) = Right (a, b)

-- parse a library name

parseLibname :: [TokenTree] -> Either Error LibName
parseLibname [] = fail "Empty library name"[]
parseLibname ((TokLeaf (TokId s p)) : []) = Right $ [s]
parseLibname ((TokLeaf (TokId s p)) : r) = parseLibname r >>= Right . (:) s
parseLibname _ = fail "Malformed library name"

-- parse a (define)

parseDefine :: [TokenTree] -> Either Error Define

-- parsing for (define id expr)

parseDefine (TokLeaf (TokId i p) : expr : []) =
    expand expr >>= \e -> return $ Define i e p

-- Parsing for (define (id args) expr...) -> (define id (lambda (args)) expr)

parseDefine (TokNode (TokLeaf (TokId i p) : args) : expr) =
    let lambda = (TokNode (TokLeaf (TokLambda p) : (TokNode args) : expr))
        parsedLambda = expand lambda
    in parsedLambda >>= \l -> return $ Define i l p

parseDefine _ = fail $ "Malformed define expression"

-- parse a list of token where a (define ...)* (expr ...)+ is expected

parseBody :: [TokenTree] -> Either Error Scope
parseBody [] = fail $ "Empty body"
parseBody (TokNode (TokLeaf (TokDefine _) : _) : []) = fail $ "Empty body"
parseBody (TokNode (TokLeaf (TokDefine _) : d@_) : r) =
    let def = parseDefine d
        scp = parseBody r
        c = mcons def scp
    in c >>= \(d, s) -> Right $ scopeAddDef s d
parseBody (_ : TokNode (TokLeaf (TokDefine _) : _) : _) = fail $
    "(define) not allowed in an expression context"
parseBody (expr : []) =
    (expand expr) >>= \e -> Right $ Scope [] $ Begin [e]
parseBody (expr : r) =
    let exp = expand expr
        scp = parseBody r
        c = mcons exp scp
    in c >>= \(e, r) -> Right $ scopeAddExpr r e

-- parser: entry point for the parser

parser :: [Token] -> Either Error AST

parser tokens = 
    case arborize tokens of
        (exprs, []) -> parseBody exprs >>= return . AST
        (_, _) -> fail "Parse error: extra tokens after parenthesis"
