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
import Imports
import Lambda
import Lexer
import Library
import Module
import Program
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

-- Parsing for (let ((e1 v1) ... (en vn)) expr)
--   -> ((lambda (e1 ... en) expr) v1 ... vn)

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

-- parse a list of tokens where a (define ...)* (expr ...)+ is expected
-- This is the body of a lambda expression and let expressions

parseBody :: [TokenTree] -> Either Error Scope

parseBody [] = fail $ "Empty body"

parseBody (TokNode (TokLeaf (TokDefine p) : _) : []) = Left $
    Error "No expression after definitions sequence" p

parseBody (TokNode (TokLeaf (TokDefine _) : d) : r) =
    let def = parseDefine d
        scp = parseBody r
        c = mcons def scp
    in c >>= \(d, s) -> Right $ scopeAddDef s d

parseBody (_ : TokNode (TokLeaf (TokDefine p) : _) : _) = Left $
    Error "expected command or expression, but found (define ... )" p

parseBody (expr : []) =
    (expand expr) >>= \e -> Right $ Scope [] $ Begin [e]

parseBody (expr : r) =
    let exp = expand expr
        scp = parseBody r
        c = mcons exp scp
    in c >>= \(e, r) -> Right $ scopeAddExpr r e

-- parse a set of imports forms that can occur at the beginning of a program
-- or a library, in an (import ... ) expression.
--   (import set) -> (import spec)+

parseImportSet :: [TokenTree] -> Either Error Imports

parseImportSet [] = fail $ "Empty import set"

parseImportSet (TokNode t : []) =
    let imp = parseImp t in
    imp >>= \t -> Right $ [t]

parseImportSet (TokNode h : t) =
    let rec = parseImportSet t
        imp = parseImp h
    in (mcons rec imp) >>= \(r, i) -> Right $ i : r


-- parse a list of identifiers

parseIdentList :: [TokenTree] -> Either Error [Identifier]

parseIdentList [] = fail $ "Empty identifier list"

parseIdentList (TokLeaf (TokId i p) : r) =
    let lst = parseIdentList r in
    lst >>= Right . (:) (Identifier i p)

parseIdentList _ = fail $ "malformed identifier list"

-- parse a single import specifier
-- formal syntax:
--   (import spec)
--     (library name)
--     (only (import set) (identifier)+)
--     (except (import set) (identifier)+)
--     (prefix (import set) (identifier)+)
--     (rename (import set) ((identifier) (identifier))+)

parseImp :: [TokenTree] -> Either Error Import

parseImp (TokLeaf (TokOnly p) : (TokNode imp) : ids) =
    let i = parseIdentList ids
        n = parseImp imp
    in (mcons i n) >>= \(i, n) -> Right $ Only n i

parseImp (TokLeaf (TokOnly p) : _) =
    Left $ Error "malformed only expression" p

parseImp (TokLeaf (TokExcept p) : (TokNode imp) : ids) =
    let i = parseIdentList ids
        n = parseImp imp
    in (mcons i n) >>= \(i, n) -> Right $ Except n i

parseImp (TokLeaf (TokExcept p) : _) =
    Left $ Error "malformed except expression" p

parseImp (TokLeaf (TokPrefix p) : (TokNode imp) : ids) =
    let i = parseIdentList ids
        n = parseImp imp
    in (mcons i n) >>= \(i, n) -> Right $ Prefix n i

parseImp (TokLeaf (TokPrefix p) : _) =
    Left $ Error "malformed except expression" p

parseImp (TokLeaf (TokRename p) : (TokNode imp) : rns) =
    let parseRenames l = case l of
            [] -> fail $ "Empty rename list"
            TokNode(TokLeaf (TokId i p1) : TokLeaf (TokId n p2) : []) : [] ->
                Right $ [(Identifier i p1, Identifier n p2)]
            TokNode(TokLeaf (TokId i p1) : TokLeaf (TokId n p2) : []) : r ->
                let l = parseRenames r in
                l >>= \l -> Right $ (Identifier i p1, Identifier n p2) : l
            _ ->  fail $ "Malformed rename list"
        n = parseImp imp
        l = parseRenames rns
    in (mcons n l) >>= \(n, i) -> Right $ Rename n i

parseImp (TokLeaf (TokRename p) : _) =
    Left $ Error "malformed rename expression" p

parseImp [] =
    fail $ "malformed import"

parseImp lst =
    parseLibname lst >>= Right . Imports.Lib

-- parse the body of a program or a library. It differs from the body of a
-- lambda by allowing (import)+ (command or definition)+
-- formal syntax:
--   (program) ->
--     (import declaration)+
--     (command or definition)+
--   (import declaration) -> (import (import set)+)

parseProgramBody :: [TokenTree] -> Either Error Program

parseProgramBody [] = fail $ "Empty program"

parseProgramBody (TokNode (TokLeaf (TokImport p) : _) : []) = Left $
    Error "no program after an import sequence" p

parseProgramBody (TokNode (TokLeaf (TokImport _) : s) : r) =
    let set = parseImportSet s
        prog = parseProgramBody r
        c = mcons set prog
    in c >>= \(s, p) -> Right $ progAddImp p s

parseProgramBody (_ : TokNode (TokLeaf (TokImport p) : _) : _) = Left $
    Error "expected command or definition, but found (import ... )" p

parseProgramBody (def : []) = case def of
    TokNode (TokLeaf (TokDefine p) : d) ->
        let def = parseDefine d in
        def >>= \d -> Right $ Program [] [Def d]
    expr ->
        let e = expand expr in
        e >>= \e -> Right $ Program [] [Cmd e]

parseProgramBody (def : r) = case def of
    TokNode (TokLeaf (TokDefine p) : d) ->
        let def = parseDefine d
            prog = parseProgramBody r
        in (mcons def prog) >>= \(d, p) -> Right $ progAddDef p d
    expr ->
        let e = expand expr
            prog = parseProgramBody r
        in (mcons e prog) >>= \(e, p) -> Right $ progAddExpr p e

-- parser: entry point for the parser

parseProgramOrLibrary :: [TokenTree] -> Either Error Module

parseProgramOrLibrary [] = fail $ "Empty program"

parseProgramOrLibrary (TokNode (TokLeaf (TokImport _) : s) : r) =
    let set = parseImportSet s
        prog = parseProgramBody r
        c = mcons set prog
    in c >>= \(s, p) -> Right $ Prog $ progAddImp p s

parseProgramOrLibrary _ =
    fail "Expected either of (import ... ) or (define-library ... )"

parser :: [Token] -> Either Error Module

parser tokens = 
    case arborize tokens of
        (exprs, []) -> parseProgramOrLibrary exprs
        (_, _) -> fail "Parse error: extra tokens after parenthesis"
