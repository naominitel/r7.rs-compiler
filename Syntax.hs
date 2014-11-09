module Syntax
(
    LetArgs,
    letBindings,
    letArgsIdentifiers,
    letArgsValues,
    lambdaArgs
) where

-- Module for basic operations on the primary tree defined in the Tree module

import Control.Monad(liftM)
import Error
import Lexer
import Tree

-- args of a let epression
-- couple (Identifier, expression)

type LetArgs = [(Token, TokenTree)]

letBindings :: TokenTree -> Either Error LetArgs
letBindings (Node bindings _) = letArgs bindings
letBindings (Leaf tok p) = Left $ Error
    ("expected bindings list, but found " ++ show tok) p

-- extracts the list of a let form formal args
-- e.g. ((e1 v1) (e2 v2)) -> [(e1, v1), (e2, v2)]

letArgs :: [TokenTree] -> Either Error LetArgs
letArgs []                                     = return []
letArgs (Node [Leaf t@(Id _) _, val] _ : rest) = liftM ((:) (t, val)) (letArgs rest)
letArgs (Node (t : _) _ : _)                   = Left $ Error
    ("expected an identifier but found " ++ first t) (ttPos t)
letArgs (Node _ p : _)                         = Left $ Error
    ("expected a pair of the form (id expr)") p
letArgs (Leaf t p : _)                          = Left $ Error
    ("expected a pair of the form (id expr) but found " ++ show t) p

-- return the list of identifiers of the formal let args
-- e.g. [e1, e2, ..., en]

letArgsIdentifiers :: LetArgs -> [Token]
letArgsIdentifiers [] = []
letArgsIdentifiers ((i, _) : r) = i : letArgsIdentifiers r

-- return the list of expressions of the formal let args
-- e.g. [v1, v2, ..., vn]

letArgsValues :: LetArgs -> [TokenTree]
letArgsValues [] = []
letArgsValues ((_, tree) : r) = tree : letArgsValues r

-- return the list of arguments of a lambda-expr
-- e.g. [a, b, c]
-- The Maybe String represents the presence of a variadic parameter
-- e.g. for (lambda (a b . c) ...) this returns ([a, b], (Just c))

lambdaArgsLst :: [TokenTree] -> Either Error ([String], Maybe String)
lambdaArgsLst [] = return ([], Nothing)
lambdaArgsLst [Leaf Dot _, Leaf (Id var) _] = return ([], Just var)
lambdaArgsLst [Leaf Dot _, t] = Left $ Error
    ("expected an identifier but found " ++ first t) (ttPos t)
lambdaArgsLst [Leaf Dot p] = Left $ Error
    ("expected an identifier after `.'") p
lambdaArgsLst (Leaf Dot _ : _ : t : _) = Left $ Error
    ("unexpected token " ++ first t) (ttPos t)
lambdaArgsLst (Leaf (Id arg) _ : r) =
    lambdaArgsLst r >>= \(a, v) -> return (arg : a, v)
lambdaArgsLst (t : _) = Left $ Error
    ("expected an identifier or `.' but found " ++ first t) (ttPos t)

lambdaArgs :: TokenTree -> Either Error ([String], Maybe String)
lambdaArgs (Node args _)     = lambdaArgsLst args
lambdaArgs (Leaf (Id arg) _) = return ([], Just arg)
lambdaArgs (Leaf t p)        = Left $ Error ("unexpected token " ++ show t) p
