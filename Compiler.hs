module Compiler
(
    CompileExpr(codegen),
    Env,
    Expression(Expr),
    Result(Failure, Pass),
    State,
    continue,
    envExtend,
    envFetch,
    initialState,
    nextLabel
) where

import Data.Word

import AST
import Bytecode
import Error

-- This module contains various definition for code generation

type Env = [String]
type State = ([Env], Word32)

-- Initial compiler state at the beginning of the compilation

initialState :: State
initialState = ([primEnv], 0)

-- Global environment for containing primitives

primEnv :: [String]
primEnv = ["+", "-", "*", "/", "=", "eq?", "list", "cons", "car", "cdr",
        "display", "newline", "set-car!", "set-cdr!", "exit"]

-- utility function used by primFetch or envFetch to return the index of an item

envSearch :: String -> Env -> Int -> Maybe Int
envSearch _ [] _ = Nothing
envSearch s (v:e) i = if s == v then Just i else envSearch s e (i + 1)

-- return the position of an identifier in the lexical environment

envFetch :: String -> [Env] -> Maybe Int
envFetch _ [] = Nothing
envFetch s (e:r) = case envSearch s e 0 of
    Just l -> Just l
    Nothing -> envFetch s r >>= Just . (+ (length e))

-- return a new usable label depending on the current label

nextLabel :: State -> (Word32, State)
nextLabel (env, lbl) = (lbl, (env, lbl + 1))

-- returns a new compiler state by adding an environment to the lexical env.

envExtend :: Env -> State -> State
envExtend e1 (e2, lbl) = (e1:e2, lbl)

-- basic class for expressions that can be compiled
-- The CompilerState type is passed to any codegen function and
-- includes information about the current context of the expression
-- being compiled:
--   * The lexical environment
--   * The next available label
--   * If the current expression is in last-call-position

data Result = Failure [Error] State | Pass [Instr] State

continue :: Result -> (State -> Result) -> ([Instr] -> [Instr] -> [Instr]) -> Result
continue ret next gen =
    case ret of
        Failure errs st ->
            let cont = next st in
            case cont of
                Failure e s -> Failure (errs ++ e) s
                Pass _ s -> Failure errs s
        Pass instrs st ->
            let cont = next st in
            case cont of
                Failure errs s -> cont
                Pass i s -> Pass (gen instrs i) s

-- Wrapper type around expression types to be used by compiler functions

data Expression = forall a. CompileExpr a => Expr a

class (Expand a) => CompileExpr a where
    codegen :: a -> State -> Bool -> Result

instance Show Expression where
    show (Expr e) = show e

instance Expand Expression where
    expand ctx (Expr a) = Expr $ expand ctx a
