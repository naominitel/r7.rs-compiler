module Lexer 
( 
    Pos(Pos),

    Token
    (
        TokOpen, 
        TokClose, 
        TokDefine,
        TokIf, 
        TokLambda,
        TokLet,  
        TokInt, 
        TokBool,
        TokStr,
        TokOther
    ),

    lexer, 
    program,
    syntaxChecker
) where

import Control.Monad.Instances
import Text.Regex.Posix

-- Pos: position of a token in the file (Line, Column)

data Pos = Pos Int Int

instance Show Pos where
    show (Pos l c) = "(Line " ++ (show l) ++ ":" ++ (show c) ++ ")"

-- Token: basic data structure for a program lexical unit.
--  * Open is an opening brace '('
--  * Close is a closing brace ')'
--  * Other is any other identifier or token

data Token 
    = TokOpen Pos 
    | TokClose Pos
    | TokDefine Pos
    | TokIf Pos
    | TokLambda Pos
    | TokLet Pos
    | TokInt Int Pos
    | TokBool Bool Pos
    | TokStr String Pos
    | TokOther String Pos

instance Show Token where
    show (TokOpen pos)    = "Open "  ++ show pos
    show (TokClose pos)   = "Close " ++ show pos
    show (TokDefine pos)  = "define " ++ show pos
    show (TokIf pos)      = "if " ++ show pos
    show (TokLambda pos)  = "lambda " ++ show pos
    show (TokLet pos)     = "let " ++ show pos
    show (TokInt i pos)   = show i
    show (TokBool b pos)  = show b
    show (TokStr s pos)   = show s
    show (TokOther t pos) = t ++ " " ++ show pos

-- Program

type Program = (String, Pos)

program :: String -> Program
program str = (str, Pos 0 0)

forward :: Pos -> Pos
forward (Pos l c) = (Pos l $ c + 1)

newline :: Pos -> Pos
newline (Pos l c) = (Pos (l + 1) c)

-- cut: split the program into string tokens

cut :: Program -> [(String, Pos)]

cut ('(':s, pos)   = ("(", pos) : cut (s, forward pos)
cut (')':s, pos)   = (")", pos) : cut (s, forward pos)
cut (' ':s, pos)   = cut (s, forward pos)
cut (a:'(':s, pos) = ([a], pos) : cut ('(':s, forward pos)
cut (a:')':s, pos) = ([a], pos) : cut (')':s, forward pos)
cut (a:' ':s, pos) = ([a], pos) : cut (' ':s, forward pos)
cut (a:[], pos)    = ([a], pos) : []
cut ([], pos)      = []
cut (a:s, pos)     = let ((n,p):r) = cut (s, forward pos) in (a:n, pos):r

-- Tokenize: core of the lexer, transforms the program into a token list

tokenize :: (String, Pos) -> Token
tokenize ("(", p)      = TokOpen p
tokenize (")", p)      = TokClose p
tokenize ("let", p)    = TokLet p
tokenize ("lambda", p) = TokLambda p
tokenize ("define", p) = TokDefine p
tokenize ("if", p)     = TokIf p
tokenize ("#t", p)     = TokBool True p
tokenize ("#f", p)     = TokBool False p
tokenize (s, p)
    | s =~ "^[0-9]+$"           = TokInt (read s :: Int) p
    -- | s =~ "\"(\\.|[^\\"])*\""  = TokStr (read s :: String) p
    | otherwise                 = TokOther s p

-- Check if the pairs of parenthesis are correct

syntaxChecker :: [Token] -> Int -> Either String [Token]
syntaxChecker [] 0                         = Right []
syntaxChecker [] _                         = fail "Unclosed parenthesis at end of input"
syntaxChecker (tok@(TokClose p):rest) count
    | (count > 0)                          = (syntaxChecker rest $ count - 1) >>= Right . (:) tok
    | otherwise                            = fail $ "Unexpected ')' at " ++ show p
syntaxChecker (tok@(TokOpen p):rest) count = (syntaxChecker rest $ count + 1) >>= Right . (:) tok
syntaxChecker (tok:rest) count             = (syntaxChecker rest $ count) >>= Right . (:) tok

-- entry point for the lexer

lexer :: Program -> [Token]
lexer prog = map tokenize $ cut prog
