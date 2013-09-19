module Compiler
(
    Env,
    CompilerState,
    initialState,
    primFetch,
    envFetch,
    nextLabel,
    envExtend,
    Expression(codegen)
) where

import Bytecode
import Data.Word

-- This module contains various definition for code generation

type Env = [String]
type CompilerState = ([Env], Word32)

-- Initial compiler state at the beginning of the compilation

initialState :: CompilerState
initialState = ([], 0)

-- Global environment for containing primitives

primEnv :: [String]
primEnv = ["+"]

-- utility function used by primFetch or envFetch to return the index of an item

envSearch :: String -> Env -> Int -> Maybe Int
envSearch _ [] _ = Nothing
envSearch s (v:e) i = if s == v then Just i else envSearch s e (i + 1)

-- return the position of the given primitive in the primitives environment

primFetch :: String -> Maybe Int
primFetch s = envSearch s primEnv 0

-- return the position of an identifier in the lexical environment

envFetch :: String -> [Env] -> Maybe Int
envFetch _ [] = Nothing
envFetch s (e:r) = case envSearch s e 0 of
    Just l -> Just l
    Nothing -> envFetch s r >>= Just . (+ (length e))

-- return a new usable label depending on the current label

nextLabel :: CompilerState -> (Word32, CompilerState)
nextLabel (env, lbl) = (lbl, (env, lbl + 1))

-- returns a new compiler state by adding an environment to the lexical env.

envExtend :: Env -> CompilerState -> CompilerState
envExtend e1 (e2, lbl) = (e1:e2, lbl)

-- basic class for expressions that can be compiled

class (Show a) => Expression a where
    codegen :: a -> CompilerState -> Either String ([Instr], CompilerState)

    