module Lexer 
( 
    Pos(Pos),

    Token
    (
        TokOpen, 
        TokClose, 
        TokBegin,
        TokDefine,
        TokIf, 
        TokLambda,
        TokLet,  
        TokInt, 
        TokBool,
        TokQuote,
        TokSet,
        TokStr,
        TokId,
        TokDot
    ),

    lexer, 
    program,
    syntaxChecker,
    tokPos
) where

import Control.Monad.Instances
import Data.Word
import Text.Regex.Posix

-- Pos: position of a token in the file (Line, Column, Filename)

data Pos = Pos Int Int String

instance Show Pos where
    show (Pos l c f) = f ++ ":" ++ (show l) ++ ":" ++ (show c) ++ ""

-- Token: basic data structure for a program lexical unit.
--  * Open is an opening brace '('
--  * Close is a closing brace ')'
--  * Other is any other identifier or token

data Token 
    = TokOpen Pos 
    | TokClose Pos
    | TokBegin Pos
    | TokDefine Pos
    | TokIf Pos
    | TokLambda Pos
    | TokLet Pos
    | TokInt Word64 Pos
    | TokBool Bool Pos
    | TokQuote Pos
    | TokSet Pos
    | TokStr String Pos
    | TokId String Pos
    | TokDot Pos

instance Show Token where
    show (TokOpen pos)    = "Open"
    show (TokClose pos)   = "Close"
    show (TokDefine pos)  = "define"
    show (TokIf pos)      = "if"
    show (TokLambda pos)  = "lambda"
    show (TokLet pos)     = "let"
    show (TokInt i pos)   = show i
    show (TokBool b pos)  = show b
    show (TokStr s pos)   = show s
    show (TokId t pos)    = t
    show (TokDot _)       = "."

tokPos :: Token -> Pos
tokPos (TokOpen pos)    = pos
tokPos (TokClose pos)   = pos
tokPos (TokDefine pos)  = pos
tokPos (TokIf pos)      = pos
tokPos (TokLambda pos)  = pos
tokPos (TokLet pos)     = pos
tokPos (TokInt _ pos)   = pos  
tokPos (TokBool _ pos)  = pos
tokPos (TokStr _ pos)   = pos
tokPos (TokId _ pos)    = pos
tokPos (TokDot pos)     = pos

-- Program

type Program = (String, Pos)

program :: String -> String -> Program
program str fname = (str, Pos 1 1 fname)

forward :: Pos -> Pos
forward (Pos l c f) = (Pos l (c + 1) f)

newline :: Pos -> Pos
newline (Pos l c f) = (Pos (l + 1) 1 f)

-- cut: split the program into string tokens

separator :: Char -> Bool
separator c 
    | [c] =~ "[ \t\r\n()]" = True
    | otherwise            = False

cut :: Program -> [(String, Pos)]

cut ([], pos)       = []
cut ('(':s, pos)    = ("(", pos) : cut (s, forward pos)
cut (')':s, pos)    = (")", pos) : cut (s, forward pos)
cut ('\n':s, pos)   = cut (s, newline pos)
cut ('\t':s, pos)   = cut (s, forward pos)
cut ('\r':s, pos)   = cut (s, forward pos)
cut (' ':s, pos)    = cut (s, forward pos)
cut (a:[], pos)     = ([a], pos) : []
cut (a:c:s, pos)
    | separator c   = ([a], pos) : cut (c:s, forward pos)
    | otherwise     = let ((n,p):r) = cut (c:s, forward pos) in (a:n, pos):r

-- Regular expressions used to tokenize the program

identifier = "^(([a-zA-Z!$%&*/:<=>?^_`-][a-zA-Z!$%&*/:<=>?^_`+.@0-9-]*)\
                        \|([+-]|\\.\\.\\.))$"

integer = "^[0-9]+$"
string = "\"(\\.|[^\\\\\"])*\""


-- Tokenize: core of the lexer, transforms the program into a token list

tokenize :: (String, Pos) -> Either String Token
tokenize (".", p)      = Right $ TokDot p
tokenize ("(", p)      = Right $ TokOpen p
tokenize (")", p)      = Right $ TokClose p
tokenize ("let", p)    = Right $ TokLet p
tokenize ("set!", p)   = Right $ TokSet p
tokenize ("lambda", p) = Right $ TokLambda p
tokenize ("define", p) = Right $ TokDefine p
tokenize ("begin", p)  = Right $ TokBegin p
tokenize ("quote", p)  = Right $ TokQuote p
tokenize ("if", p)     = Right $ TokIf p
tokenize ("#t", p)     = Right $ TokBool True p
tokenize ("#f", p)     = Right $ TokBool False p
tokenize (s, p)
    | s =~ integer     = Right $ TokInt (read s :: Word64) p
    | s =~ string      = Right $ TokStr (read s :: String) p
    | s =~ identifier  = Right $ TokId s p
    | otherwise        = Left $ "Unknown token " ++ s ++ " at " ++ show p
     
-- Check if the pairs of parenthesis are correct

syntaxChecker :: [Token] -> Int -> Either String [Token]
syntaxChecker [] 0                = Right []
syntaxChecker [] _                = Left "Unclosed parenthesis at end of input"
syntaxChecker (t@(TokOpen _):r) c = syntaxChecker r (c + 1) >>= Right . (:) t
syntaxChecker (t@(TokClose p):r) c
    | (c > 0)                     = syntaxChecker r (c - 1) >>= Right . (:) t
    | otherwise                   = Left $ "Unexpected ')' at " ++ show p
syntaxChecker (t:r) count         = syntaxChecker r count >>= Right . (:) t

-- entry point for the lexer

lexer :: Program -> Either String [Token]
lexer prog = mapM tokenize $ cut prog
