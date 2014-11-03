module Syntax
(
    LetArgs,
    letArgs,
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

-- extracts the list of a let form formal args
-- e.g. ((e1 v1) (e2 v2)) -> [(e1, v1), (e2, v2)]

letArgs :: [TokenTree] -> Either Error LetArgs
letArgs []                                            = return []
letArgs (TokNode [TokLeaf t@(TokId _ _), val] : rest) = liftM ((:) (t, val)) (letArgs rest)
letArgs _                                             = fail "Malformed let parameter list"

-- return the list of identifiers of the formal let args
-- e.g. [e1, e2, ..., en]

letArgsIdentifiers :: LetArgs -> Either Error [Token]
letArgsIdentifiers [] = return []
letArgsIdentifiers ((i, _) : r) = liftM ((:) i) (letArgsIdentifiers r)

-- return the list of expressions of the formal let args
-- e.g. [v1, v2, ..., vn]

letArgsValues :: LetArgs -> Either Error [TokenTree]
letArgsValues [] = return []
letArgsValues ((_, tree) : r) = liftM ((:) tree) (letArgsValues r)

-- return the list of arguments of a lambda-expr
-- e.g. [a, b, c]
-- The Maybe String represents the presence of a variadic parameter
-- e.g. for (lambda (a b . c) ...) this returns ([a, b], (Just c))

lambdaArgs :: [TokenTree] -> Either Error ([String], Maybe String)
lambdaArgs [] = return ([], Nothing)
lambdaArgs (TokLeaf (TokId arg _) : r) =
    lambdaArgs r >>= \(a, v) -> return (arg : a, v)
lambdaArgs (TokLeaf (TokDot _) : [TokLeaf (TokId var _)]) = return ([], Just var)
lambdaArgs _ = fail "Malformed lambda parameter list"
