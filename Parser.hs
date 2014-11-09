-- parser.hs
-- Copyright (c) Naomi Nitel <naominitel@gmail.com>

module Parser
(
    arborize,
    parseExpr,
    parser
) where

import Control.Monad (liftM)

import Apply
import Begin
import Compiler
import Constant
import Datum
import Define
import Error
import Identifier
import If
import Imports
import Lambda
import Library
import Module
import Program
import Quote
import Set
import Syntax
import Tree

import qualified Lexer as L
import Lexer (Token ())

-- The parser first transforms the flat token list into a tree structure.
-- Braces are removed, and any other token is a leaf or node of the tree.
-- Any token has attached its contents and position in file

arborize :: [(Token, Pos)] -> ([TokenTree], [(Token, Pos)])

arborize [] = ([], [])

arborize ((L.Close, _) : s) = ([], s)

arborize ((L.Open, p) : s) =
    let (rec, cont) = arborize s
        (next, end) = arborize cont
    in (Node rec p : next, end)

arborize ((tok, p) : s) =
    let (rec, cont) = arborize s
    in (Leaf tok p : rec, cont)

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

parseExpr (Node [Leaf L.Let _, bindings, expr] p) = do
    args <- letBindings bindings
    let ids    = letArgsIdentifiers args
    let vals   = letArgsValues args
    let lambda = Node [Leaf L.Lambda p, Node (map (`Leaf` p) ids) p, expr] p
    parseExpr $ Node (lambda : vals) p

parseExpr (Node (Leaf L.Let _ : _) p) = Left $ Error "Malformed let-expression" p

-- Parsing for (lambda (e1 ... en) expr)

parseExpr (Node (Leaf L.Lambda lp : args : exprs) p) = do
    (args, var) <- lambdaArgs args
    body <- parseBody exprs lp
    Right $ Expr $ Lambda args var body p

parseExpr (Node (Leaf L.Lambda _ : _) p) =
    Left $ Error "Malformed lambda-expression" p

-- Parsing for (set! id val)

parseExpr (Node [Leaf L.Set p, Leaf (L.Id i) _, expr] _) =
    parseExpr expr >>= \e -> return $ Expr $ Set i e p

parseExpr (Node (Leaf L.Set _ : Leaf (L.Id _) _ : _ : t : _) _) =
    Left $ Error ("unexpected token " ++ first t) (ttPos t)

parseExpr (Node [Leaf L.Set _, Leaf (L.Id _) p] _) =
    Left $ Error ("expression expected after identifier") p

parseExpr (Node (Leaf L.Set _ : t : _) _) =
    Left $ Error ("expected an identifier, but found " ++ first t) (ttPos t)

parseExpr (Node [Leaf L.Set p] _) =
    Left $ Error "identifier expected after `set!'" p

-- Parsing for (if expr-bool expr-if expr-else)

parseExpr (Node [Leaf L.If p, exprif, exprt, exprf] _) = do
    parsedif <- parseExpr exprif
    parsedt <- parseExpr exprt
    parsedf <- parseExpr exprf
    Right $ Expr $ If parsedif parsedt parsedf p

parseExpr (Node (Leaf L.If _ : _ : _ : _ : t : _) _) =
    Left $ Error ("unexpected token " ++ first t) (ttPos t)

parseExpr (Node (Leaf L.If p : _) _) =
    Left $ Error "not enough arguments to if-expression" p

-- Parsing for (quote expr)

parseExpr (Node [Leaf L.Quote p, expr] _) =
    Right $ Expr $ Quote (getDatum expr) p

parseExpr (Node (Leaf L.Quote _ : _) p) =
    Left $ Error "malformed quote-expression" p

-- Parsing for (begin expr)

parseExpr (Node (Leaf L.Begin _ : exprs) _) = do
    parsedExprs <- mapM parseExpr exprs
    Right $ Expr $ Begin parsedExprs

-- Parsing for function application (f a1 a2 a3)

parseExpr (Node (func : args) _) = do
    parsedFunc <- parseExpr func
    parsedArgs <- mapM parseExpr args
    Right $ Expr $ Apply parsedFunc parsedArgs

parseExpr (Node [] p) =
    Left $ Error "empty application" p

-- Parsing for anything else, like "var" or "1"

parseExpr (Leaf (L.Bool b) p) = return $ Expr $ BoolConstant b p
parseExpr (Leaf (L.Int i) p)  = return $ Expr $ IntConstant i p
parseExpr (Leaf (L.Id s) p)   = return $ Expr $ Identifier s p

parseExpr (Leaf t p) =
    Left $ Error ("expected an expression, but found: " ++ show t) p

-- parse a list of identifiers

parseIdentList :: [TokenTree] -> Pos -> Either Error [Identifier]

parseIdentList [] p = Left $ Error "identifier list expected" p

parseIdentList (Leaf (L.Id i) p : r) rp =
    parseIdentList r rp >>= Right . (:) (Identifier i p)

parseIdentList (t : _) _ =
    Left $ Error ("expected identifier but found" ++ first t) (ttPos t)

-- parse a library name

parseLibname :: TokenTree -> Either Error LibName

parseLibname (Leaf t p) =
    Left $ Error ("expected library-name, but found " ++ show t) p

parseLibname (Node [] p) =
    Left $ Error "empty library-name" p

parseLibname (Node l p) = parseIdentList l p

-- parse a (define)

parseDefine :: [TokenTree] -> Pos -> Either Error Define

parseDefine [] p = Left $ Error "identifier expected after `define'" p

-- parsing for (define id expr)

parseDefine [Leaf (L.Id i) p, expr] _ =
    parseExpr expr >>= \e -> return $ Define i e p

parseDefine (Leaf (L.Id _) _ : _ : t : _) _ =
    Left $ Error ("unexpected token " ++ first t) (ttPos t)

parseDefine [Leaf (L.Id _) p] _ =
    Left $ Error ("expected expression after identifier") p

parseDefine (Leaf t p : _) _ =
    Left $ Error ("expected identifier or function but found " ++ show t) p

-- Parsing for (define (id args) expr...) -> (define id (lambda (args)) expr)

parseDefine [Node (Leaf (L.Id _) _ : _) p] _ =
    Left $ Error ("expected expression after function-name") p

parseDefine (Node (Leaf (L.Id i) p : args) lp : expr) _ = do
    let lambda = Node (Leaf L.Lambda p : Node args p : expr) lp
    parsedLambda <- parseExpr lambda
    Right $ Define i parsedLambda p

parseDefine (Node (t : _) _ : _) _ =
    Left $ Error ("expected function-name but found " ++ first t) (ttPos t)

parseDefine (Node [] p : _) _ =
    Left $ Error "function-name expected" p

-- parse a list of tokens where a (define ...)* (expr ...)+ is expected
-- This is the body of a lambda expression and let expressions

parseBody :: [TokenTree] -> Pos -> Either Error Scope

parseBody [] p = Left $ Error "expression expected" p

parseBody [Node (Leaf L.Define p : _) _] _ =
    Left $ Error "no expression after definitions sequence" p

parseBody (Node (Leaf L.Define dp : d) _ : r) p = do
    def <- parseDefine d dp
    scp <- parseBody r p
    Right $ scopeAddDef scp def

parseBody exprs _ = do
    exprs <- mapM parseExpr exprs
    Right $ Scope [] $ Begin exprs

-- parse a single import specifier
-- formal syntax:
--   (import spec)
--     (library name)
--     (only (import set) (identifier)+)
--     (except (import set) (identifier)+)
--     (prefix (import set) (identifier)+)
--     (rename (import set) ((identifier) (identifier))+)

parseImport :: TokenTree -> Either Error Import

parseImport (Leaf t p) =
    Left $ Error ("expected an import specifier, but found: " ++ show t) p

parseImport (Node (Leaf L.Only _ : imp : ids) _) = do
    i <- parseIdentList ids (ttPos imp)
    n <- parseImport imp
    Right $ Only n i

parseImport (Node (Leaf L.Only p : _) _) = do
    Left $ Error "not enough arguments to only-expression" p

parseImport (Node (Leaf L.Except p : imp : ids) _) = do
    i <- parseIdentList ids p
    n <- parseImport imp
    Right $ Except n i

parseImport (Node (Leaf L.Except p : _) _) = do
    Left $ Error "malformed except expression" p

parseImport (Node (Leaf L.Prefix p : imp : ids) _) = do
    i <- parseIdentList ids p
    n <- parseImport imp
    Right $ Prefix n i

parseImport (Node (Leaf L.Prefix p : _) _) = do
    Left $ Error "malformed except expression" p

parseImport (Node (Leaf L.Rename p : imp : rns) _) =
    let parseRenames l p = case l of
            [] -> Left $ Error "rename-list expected" p
            [Node [Leaf (L.Id i) p1, Leaf (L.Id n) p2] _] ->
                Right $ [(Identifier i p1, Identifier n p2)]
            (Node [Leaf (L.Id i) p1, Leaf (L.Id n) p2] _ : r) -> do
                l <- parseRenames r p
                Right $ (Identifier i p1, Identifier n p2) : l
            (t : _) -> Left $ Error "malformed rename-list" (ttPos t)
    in do
        n <- parseImport imp
        l <- parseRenames rns p
        Right $ Rename n l

parseImport (Node (Leaf L.Rename p : _) _) = do
    Left $ Error "malformed rename expression" p

parseImport node = parseLibname node >>= Right . Imports.Lib

-- parse a set of imports forms that can occur at the beginning of a program
-- or a library, in an (import ... ) expression.
--   (import set) -> (import spec)+

parseImports :: [TokenTree] -> Pos -> Either Error Imports

parseImports [] p = Left $ Error "expected import specification" p

parseImports [t] _ = parseImport t >>= \t -> Right [t]

parseImports (h : t) p = do
    rec <- parseImports t p
    imp <- parseImport h
    Right $ imp : rec

-- parse a sequence of definitions or expressions

parseCommandsOrDef :: [TokenTree] -> Either Error Program

parseCommandsOrDef [Node (Leaf L.Define p : def) _] =
    parseDefine def p >>= \d -> Right $ Program [] [Def d]

parseCommandsOrDef (Node (Leaf L.Define p : def) _ : r) = do
    def <- parseDefine def p
    prog <- parseProgramBody r
    Right $ progAddDef prog def

parseCommandsOrDef [expr] =
    parseExpr expr >>= \e -> Right $ Program [] [Cmd e]

parseCommandsOrDef (expr : r) = do
    e <- parseExpr expr
    prog <- parseProgramBody r
    Right $ progAddExpr prog e

parseCommandsOrDef [] = Right $ Program [] []

-- parse the body of a program or a library. It differs from the body of a
-- lambda by allowing (import)+ (command or definition)+
-- formal syntax:
--   (program) ->
--     (import declaration)+
--     (command or definition)+
--   (import declaration) -> (import (import set)+)

parseProgramBody :: [TokenTree] -> Either Error Program

parseProgramBody [] = fail $ "Empty program"

parseProgramBody [Node (Leaf L.Import _ : _) p] =
    Left $ Error "no program after an import sequence" p

parseProgramBody (Node (Leaf L.Import p : imports) _ : r) = do
    set <- parseImports imports p
    prog <- parseProgramBody r
    Right $ progAddImp prog set

parseProgramBody exprs = parseCommandsOrDef exprs

-- In the context of a library declaration, a begin expression groups several
-- commands (expressions whose result is unused) and definitions

parseLibBegin :: [TokenTree] -> Library -> Either Error Library

parseLibBegin [] l = Right l

parseLibBegin (Node (Leaf L.Define p : d) _ : r) l = do
    def <- parseDefine d p
    lib <- parseLibBegin r l
    Right $ libAddDef lib def

parseLibBegin (expr : r) l = do
    e <- parseExpr expr
    lib <- parseLibBegin r l
    Right $ libAddExpr lib e

-- Parse a sequence of library declarations
-- Currently supported library declarations are import specifications or begin
-- declaration (see above)

parseLibDecl :: [TokenTree] -> Library -> Either Error Library

parseLibDecl [] l = Right $ l

parseLibDecl (Node (Leaf L.Import p : imports) _ : r) l = do
    lib <- parseLibDecl r l
    imp <- parseImports imports p
    Right $ libAddImp lib imp

parseLibDecl (Node (Leaf L.Begin _ : b) _ : r) l =
    parseLibDecl r l >>= parseLibBegin b

parseLibDecl (Node (t : _) _ : _) _ = Left $ Error
    ("expected `import' or `begin' but found: " ++ first t) (ttPos t)

parseLibDecl (Node [] p : _) _ = Left $ Error
    ("expected `import' or `begin'") p

parseLibDecl (Leaf t p : _) _ =
    Left $ Error ("unexpected token: " ++ show t) p

-- parser: entry point for the parser

parseProgramOrLibrary :: [TokenTree] -> Either Error Module

parseProgramOrLibrary (Node (Leaf L.Deflib _ : name : decls) _ : _) = do
    name <- parseLibname name
    liftM Module.Lib $ parseLibDecl decls $ Library name [] []

parseProgramOrLibrary (Node [Leaf L.Deflib p] _ : _) =
    Left $ Error "expected library name after `define-library'" p

parseProgramOrLibrary t =
    parseProgramBody t >>= \p -> Right $ Prog p

parser :: [(Token, Pos)] -> Either Error Module

parser tokens =
    case arborize tokens of
        (exprs, []) -> parseProgramOrLibrary exprs
        (_, _) -> fail "Parse error: extra tokens after parenthesis"
