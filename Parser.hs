-- parser.hs
-- Copyright (c) Naomi Nitel <naominitel@gmail.com>

module Parser 
( 
    arborize, 
    parseExpr,
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
-- Expanding rules are detailed in doc/parseExpring.

-- The data structures used here are defined in the AST module

-- parseExpr: core of the parser. expands a TokenTree into an AST, which is
-- basically the same thing with token sequences assembled into semantic objects
-- Some langage constructs are translated to others.

parseExpr :: TokenTree -> Either Error Expression

-- Parsing for (let ((e1 v1) ... (en vn)) expr)
--   -> ((lambda (e1 ... en) expr) v1 ... vn)

parseExpr (TokNode (TokLeaf (TokLet p) : (TokNode bindings) : expr : [])) = do
    args <- letArgs bindings
    ids <- letArgsIdentifiers args
    vals <- letArgsValues args
    let lambda = TokNode $ 
            TokLeaf (TokLambda p) : (TokNode $ map TokLeaf ids) : expr : []
    parseExpr $ TokNode (lambda : vals)

parseExpr (TokNode (TokLeaf (TokLet p) : _)) =
    Left $ Error "Malformed let-expr" p

-- Parsing for (lambda (e1 ... en) expr)

parseExpr (TokNode (TokLeaf (TokLambda p) : (TokNode lst) : exprs)) = do
    (args, var) <- lambdaArgs lst
    body <- parseBody exprs
    Right $ Expr $ Lambda args var body p

parseExpr (TokNode (TokLeaf (TokLambda p) : _)) =
    Left $ Error "Malformed lambda-expr" p

-- Parsing for (set! id val)

parseExpr (TokNode (TokLeaf (TokSet _) : (TokLeaf (TokId i p)) : expr : [])) =
    parseExpr expr >>= \e -> return $ Expr $ Set i e p

parseExpr (TokNode (TokLeaf (TokSet p) : _)) =
    Left $ Error "Syntax error: malformed set!-expression" p

-- Parsing for (if expr-bool expr-if expr-else)

parseExpr (TokNode (TokLeaf (TokIf p) : exprif : exprt : exprf : [])) = do
    parsedif <- parseExpr exprif
    parsedt <- parseExpr exprt
    parsedf <- parseExpr exprf
    Right $ Expr $ If parsedif parsedt parsedf p

parseExpr (TokNode (TokLeaf (TokIf p) : _)) =
    Left $ Error "Malformed if-expression" p

-- Parsing for (quote expr)

parseExpr (TokNode (TokLeaf (TokQuote p) : expr : [])) =
    Right $ Expr $ Quote (getDatum expr) p

parseExpr (TokNode (TokLeaf (TokQuote p) : _)) =
    Left $ Error "Malformed quote-expression" p

-- Parsing for (begin expr)

parseExpr (TokNode (TokLeaf (TokBegin p) : exprs)) = do
    parsedExprs <- mapM parseExpr exprs
    Right $ Expr $ Begin parsedExprs

-- Parsing for function application (f a1 a2 a3)

parseExpr (TokNode (func : args)) = do
    parsedFunc <- parseExpr func
    parsedArgs <- mapM parseExpr args
    Right $ Expr $ Apply parsedFunc parsedArgs

-- Parsing for anything else, like "var" or "1"

parseExpr (TokLeaf (TokBool b p))  = return $ Expr $ BoolConstant b p
parseExpr (TokLeaf (TokInt i p))   = return $ Expr $ IntConstant i p
parseExpr (TokLeaf (TokId s p))    = return $ Expr $ Identifier s p

parseExpr (TokLeaf t) =
    Left $ Error ("expected an expression, but found: " ++ show t) (tokPos t)

-- parse a library name

parseLibname :: [TokenTree] -> Either Error LibName
parseLibname [] = Right []
parseLibname (TokLeaf (TokId s p) : []) = Right $ [s]
parseLibname (TokLeaf (TokId s p) : r) = parseLibname r >>= Right . (:) s
parseLibname (TokLeaf t : _) = Left $ Error
    ("unexpected token in library name: " ++ show t) (tokPos t)
parseLibname (TokNode (TokLeaf t : _) : _) = Left $ Error
    ("unexpected token in library name: " ++ show t) (tokPos t)

-- parse a (define)

parseDefine :: [TokenTree] -> Either Error Define

-- parsing for (define id expr)

parseDefine (TokLeaf (TokId i p) : expr : []) =
    parseExpr expr >>= \e -> return $ Define i e p

-- Parsing for (define (id args) expr...) -> (define id (lambda (args)) expr)

parseDefine (TokNode (TokLeaf (TokId i p) : args) : expr) = do
    let lambda = (TokNode (TokLeaf (TokLambda p) : (TokNode args) : expr))
    parsedLambda <- parseExpr lambda
    Right $ Define i parsedLambda p

parseDefine _ = fail $ "Malformed define expression"

-- parse a list of tokens where a (define ...)* (expr ...)+ is expected
-- This is the body of a lambda expression and let expressions

parseBody :: [TokenTree] -> Either Error Scope

parseBody [] = fail $ "Empty body"

parseBody (TokNode (TokLeaf (TokDefine p) : _) : []) = Left $
    Error "No expression after definitions sequence" p

parseBody (TokNode (TokLeaf (TokDefine _) : d) : r) = do
    def <- parseDefine d
    scp <- parseBody r
    Right $ scopeAddDef scp def

parseBody (_ : TokNode (TokLeaf (TokDefine p) : _) : _) = Left $
    Error "expected command or expression, but found (define ... )" p

parseBody (expr : []) =
    (parseExpr expr) >>= \e -> Right $ Scope [] $ Begin [e]

parseBody (expr : r) = do
    exp <- parseExpr expr
    scp <- parseBody r
    Right $ scopeAddExpr scp exp

-- parse a set of imports forms that can occur at the beginning of a program
-- or a library, in an (import ... ) expression.
--   (import set) -> (import spec)+

parseImportSet :: [TokenTree] -> Either Error Imports

parseImportSet [] = Right $ []

parseImportSet (TokNode t : []) =
    parseImp t >>= \t -> Right $ [t]

parseImportSet (TokNode h : t) = do
    rec <- parseImportSet t
    imp <- parseImp h
    Right $ imp : rec

parseImportSet (TokLeaf t : _) = Left $ Error
    ("parse error: expected an import specifier, but found: " ++ show t) (tokPos t)

parseImports :: TokenTree -> Either Error Imports

parseImports (TokNode (TokLeaf (TokImport p) : lst)) =
    case parseImportSet lst of
        Left err -> Left $ err
        Right [] -> Left $ Error "empty import set" p
        Right l  -> Right $ l

parseImports (TokLeaf tok) = Left $ Error
    ("parse error: expected (import ... ), but found: " ++ show tok) (tokPos tok)

-- parse a list of identifiers

parseIdentList :: [TokenTree] -> Either Error [Identifier]

parseIdentList [] = fail $ "Empty identifier list"

parseIdentList (TokLeaf (TokId i p) : r) =
    parseIdentList r >>= Right . (:) (Identifier i p)

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

parseImp (TokLeaf (TokOnly p) : (TokNode imp) : ids) = do
    i <- parseIdentList ids
    n <- parseImp imp
    Right $ Only n i

parseImp (TokLeaf (TokOnly p) : _) =
    Left $ Error "malformed only expression" p

parseImp (TokLeaf (TokExcept p) : (TokNode imp) : ids) = do
    i <- parseIdentList ids
    n <- parseImp imp
    Right $ Except n i

parseImp (TokLeaf (TokExcept p) : _) =
    Left $ Error "malformed except expression" p

parseImp (TokLeaf (TokPrefix p) : (TokNode imp) : ids) = do
    i <- parseIdentList ids
    n <- parseImp imp
    Right $ Prefix n i

parseImp (TokLeaf (TokPrefix p) : _) =
    Left $ Error "malformed except expression" p

parseImp (TokLeaf (TokRename p) : (TokNode imp) : rns) =
    let parseRenames l = case l of
            [] -> fail $ "Empty rename list"
            TokNode(TokLeaf (TokId i p1) : TokLeaf (TokId n p2) : []) : [] ->
                Right $ [(Identifier i p1, Identifier n p2)]
            TokNode(TokLeaf (TokId i p1) : TokLeaf (TokId n p2) : []) : r -> do
                l <- parseRenames r
                Right $ (Identifier i p1, Identifier n p2) : l
            _ ->  fail $ "Malformed rename list"
    in do
        n <- parseImp imp
        l <- parseRenames rns
        Right $ Rename n l

parseImp (TokLeaf (TokRename p) : _) =
    Left $ Error "malformed rename expression" p

parseImp [] = fail $ "malformed import"

parseImp lst = parseLibname lst >>= Right . Imports.Lib

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

parseProgramBody (i@(TokNode (TokLeaf (TokImport _) : s)) : r) = do
    set <- parseImports i
    prog <- parseProgramBody r
    Right $ progAddImp prog set

parseProgramBody (_ : TokNode (TokLeaf (TokImport p) : _) : _) = Left $
    Error "expected command or definition, but found (import ... )" p

parseProgramBody (def : []) = case def of
    TokNode (TokLeaf (TokDefine p) : d) ->
        parseDefine d >>= \d -> Right $ Program [] [Def d]
    expr ->
        parseExpr expr >>= \e -> Right $ Program [] [Cmd e]

parseProgramBody (def : r) = case def of
    TokNode (TokLeaf (TokDefine p) : d) -> do
        def <- parseDefine d
        prog <- parseProgramBody r
        Right $ progAddDef prog def
    expr -> do
        e <- parseExpr expr
        prog <- parseProgramBody r
        Right $ progAddExpr prog e

-- Parse a library declaration
-- formal syntax:
--   (library_declaration) ->

parseLibBegin :: [TokenTree] -> Library -> Either Error Library

parseLibBegin [] l = Right l

parseLibBegin (TokNode (TokLeaf (TokDefine _) : d) : r) l = do
    def <- parseDefine d
    lib <- parseLibBegin r l
    Right $ libAddDef lib def

parseLibBegin (expr : r) l = do
    e <- parseExpr expr
    lib <- parseLibBegin r l
    Right $ libAddExpr lib e

parseLibDecl :: [TokenTree] -> Library -> Either Error Library

parseLibDecl [] l = Right $ l

parseLibDecl (i@(TokNode (TokLeaf (TokImport _) : _)) : r) l = do
    lib <- parseLibDecl r l
    imp <- parseImports i
    Right $ libAddImp lib imp

parseLibDecl (TokNode (TokLeaf (TokBegin _) : b) : r) l =
    parseLibDecl r l >>= parseLibBegin b

-- Parse a library definition.
-- formal syntax:
--   (library_definition) ->
--     (library name)
--     (library_declaration)*

parseLib :: [TokenTree] -> Either Error Library

parseLib (TokNode ln : r) = do
    name <- parseLibname ln
    parseLibDecl r $ Library name [] []

-- parser: entry point for the parser

parseProgramOrLibrary :: [TokenTree] -> Either Error Module

parseProgramOrLibrary [] = fail $ "Empty program"

parseProgramOrLibrary (i@(TokNode (TokLeaf (TokImport _) : s)) : r) = do
    set <- parseImports i
    prog <- parseProgramBody r
    Right $ Prog $ progAddImp prog set

parseProgramOrLibrary (TokNode (TokLeaf (TokDeflib _) : l) : _) =
    parseLib l >>= \lib -> Right $ Module.Lib $ lib

parseProgramOrLibrary t =
    parseProgramBody t >>= \p -> Right $ Prog p

parser :: [Token] -> Either Error Module

parser tokens = 
    case arborize tokens of
        (exprs, []) -> parseProgramOrLibrary exprs
        (_, _) -> fail "Parse error: extra tokens after parenthesis"
