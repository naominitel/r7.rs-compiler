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

import Debug.Trace

-- args of a let epression
-- couple (Identifier, expression)

type LetArgs = [(Token, TokenTree)]

-- extracts the list of a let form formal args 
-- e.g. ((e1 v1) (e2 v2)) -> [(e1, v1), (e2, v2)]

letArgs :: [TokenTree] -> Either String LetArgs
letArgs []                                                 = return []
letArgs (TokNode (TokLeaf t@(TokOther _ _):val:[]) : rest) = letArgs rest >>= return . (:) (t, val)
letArgs _                                                  = fail "Malformed let parameter list"

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

lambdaArgs :: [TokenTree] -> Either String [String]
lambdaArgs [] = return []
lambdaArgs (TokLeaf (TokOther arg _) : r) = (lambdaArgs r) >>= return . (:) arg
lambdaArgs a = trace (show a) $ fail "Malformed lambda parameter list"