module Syntax 
(
    LetArgs,
    letArgs,
    letArgsIdentifiers,
    letArgsValues,
    lambdaArgs
) where

-- Module for basic operations on the primary tree defined in the Tree module

import Lexer
import Tree

-- args of a let epression
-- couple (Identifier, expression)

type LetArgs = [(Token, TokenTree)]

-- extracts the list of a let form formal args 
-- e.g. ((e1 v1) (e2 v2)) -> [(e1, v1), (e2, v2)]

letArgs :: [TokenTree] -> Either String LetArgs
letArgs []                                              = return []
letArgs (TokNode (TokLeaf t@(TokId _ _):val:[]) : rest) = letArgs rest >>= return . (:) (t, val)
letArgs _                                               = fail "Malformed let parameter list"

-- return the list of identifiers of the formal let args 
-- e.g. [e1, e2, ..., en]

letArgsIdentifiers :: LetArgs -> Either String [Token]
letArgsIdentifiers [] = return []
letArgsIdentifiers ((i, tree) : r) = letArgsIdentifiers r >>= return . (:) i

-- return the list of expressions of the formal let args
-- e.g. [v1, v2, ..., vn]

letArgsValues :: LetArgs -> Either String [TokenTree]
letArgsValues [] = return []
letArgsValues ((i, tree) : r) = letArgsValues r >>= return . (:) tree

-- return the list of arguments of a lambda-expr
-- e.g. [a, b, c]
-- The Maybe String represents the presence of a variadic parameter
-- e.g. for (lambda (a b . c) ...) this returns ([a, b], (Just c))

lambdaArgs :: [TokenTree] -> Either String ([String], Maybe String)
lambdaArgs [] = return ([], Nothing)
lambdaArgs (TokLeaf (TokId arg _) : r) = (lambdaArgs r) >>= \(a, v) ->
            return $ (arg : a, v)
lambdaArgs (TokLeaf (TokDot _) : (TokLeaf (TokId var _)) : []) =
            return ([], (Just var))
lambdaArgs a = fail "Malformed lambda parameter list"
