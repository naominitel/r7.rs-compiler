module Env( Env, envFetch, envExtend ) where

import AST

-- Basic data structure representing lexical environnement at a given point
-- of the program. It is represented as a list of (string, value) pairs, which
-- binds the name of a variable to its value.

type Env = [(String, Value)]

-- Fetch the value of a variable in the environnement. If the value is not found
-- return Nothing. If the variable is present twice, the first one is returned,
-- thus the beginning of the list has higher lexical precedence.

envFetch :: String -> Env -> Maybe Value
envFetch _ []            = Nothing
envFetch s ((e, v):rest) = if s == e then Just v else envFetch s rest

-- Add a variable binding at the beginning of the env. If the variable is
-- alreay present, the new value takes precedences. This function can thus be
-- used to create a scope that hides the parent lexical scope.

envExtend :: Env -> String -> Value -> Env
envExtend env a val = (a, val) : env
