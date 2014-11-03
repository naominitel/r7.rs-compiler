module Lexer
(

    Token
    (
        TokBegin,
        TokBool,
        TokClose,
        TokDefine,
        TokDeflib,
        TokDot,
        TokExcept,
        TokId,
        TokIf,
        TokImport,
        TokInt,
        TokLambda,
        TokLet,
        TokOnly,
        TokOpen,
        TokPrefix,
        TokQuote,
        TokRename,
        TokSet,
        TokStr
    ),

    lexer,
    program,
    syntaxChecker,
    tokPos
) where

import Data.Word
import Text.Regex.Posix

import Error
import Result

type LexResult = Result [Error] [Token] ()

-- Token: basic data structure for a program lexical unit.
--  * Open is an opening brace '('
--  * Close is a closing brace ')'
--  * others are any other identifier or token

data Token
    = TokBegin Pos
    | TokBool Bool Pos
    | TokClose Pos
    | TokDefine Pos
    | TokDeflib Pos
    | TokDot Pos
    | TokExcept Pos
    | TokId String Pos
    | TokIf Pos
    | TokInt Word64 Pos
    | TokImport Pos
    | TokLambda Pos
    | TokLet Pos
    | TokOnly Pos
    | TokOpen Pos
    | TokPrefix Pos
    | TokQuote Pos
    | TokRename Pos
    | TokSet Pos
    | TokStr String Pos

instance Show Token where
    show (TokBegin _)   = "begin"
    show (TokDefine _)  = "define"
    show (TokDeflib _)  = "define-library"
    show (TokExcept _)  = "except"
    show (TokIf _)      = "if"
    show (TokImport _)  = "import"
    show (TokLambda _)  = "lambda"
    show (TokLet _)     = "let"
    show (TokOnly _)    = "only"
    show (TokPrefix _)  = "prefix"
    show (TokQuote _)   = "quote"
    show (TokRename _)  = "rename"
    show (TokSet _)     = "set!"
    show (TokDot _)     = "."
    show (TokOpen _)    = "("
    show (TokClose _)   = ")"
    show (TokBool b _)  = show b
    show (TokInt i _)   = show i
    show (TokStr s _)   = show s
    show (TokId t _)    = t

tokPos :: Token -> Pos
tokPos (TokBegin pos)   = pos
tokPos (TokBool _ pos)  = pos
tokPos (TokClose pos)   = pos
tokPos (TokDefine pos)  = pos
tokPos (TokDeflib pos)  = pos
tokPos (TokExcept pos)  = pos
tokPos (TokDot pos)     = pos
tokPos (TokId _ pos)    = pos
tokPos (TokIf pos)      = pos
tokPos (TokImport pos)  = pos
tokPos (TokInt _ pos)   = pos
tokPos (TokLambda pos)  = pos
tokPos (TokLet pos)     = pos
tokPos (TokOnly pos)    = pos
tokPos (TokOpen pos)    = pos
tokPos (TokPrefix pos)  = pos
tokPos (TokQuote pos)   = pos
tokPos (TokRename pos)  = pos
tokPos (TokSet pos)     = pos
tokPos (TokStr _ pos)   = pos

-- Program

type Program = (String, Pos)

program :: String -> String -> Program
program str fname = (str, Pos 1 1 fname)

forward :: Pos -> Pos
forward (Pos l c f) = Pos l (c + 1) f

newline :: Pos -> Pos
newline (Pos l _ f) = Pos (l + 1) 1 f

-- cut: split the program into string tokens

separator :: Char -> Bool
separator c
    | [c] =~ "[ \t\r\n()]" = True
    | otherwise            = False

cut :: Program -> [(String, Pos)]
cut ([], _)         = []
cut ('(':s, pos)    = ("(", pos) : cut (s, forward pos)
cut (')':s, pos)    = (")", pos) : cut (s, forward pos)
cut ('\n':s, pos)   = cut (s, newline pos)
cut ('\t':s, pos)   = cut (s, forward pos)
cut ('\r':s, pos)   = cut (s, forward pos)
cut (' ':s, pos)    = cut (s, forward pos)
cut ([a], pos)      = [([a], pos)]
cut (a:c:s, pos)
    | separator c   = ([a], pos) : cut (c:s, forward pos)
    | otherwise     = let ((n,_):r) = cut (c:s, forward pos) in (a:n, pos):r

-- Regular expressions used to tokenize the program

identifier :: String
identifier = "^(([a-zA-Z!$%&*/:<=>?^_`-][a-zA-Z!$%&*/:<=>?^_`+.@0-9-]*)\
                        \|([+-]|\\.\\.\\.))$"

integer :: String
integer = "^[0-9]+$"

string :: String
string = "\"(\\.|[^\\\\\"])*\""

-- Tokenize: core of the lexer, transforms the program into a token list

tokenize :: (String, Pos) -> Either Error Token

-- Special tokens

tokenize (".", p)              = Right $ TokDot p
tokenize ("(", p)              = Right $ TokOpen p
tokenize (")", p)              = Right $ TokClose p

-- Keywords

tokenize ("begin", p)          = Right $ TokBegin p
tokenize ("define", p)         = Right $ TokDefine p
tokenize ("define-library", p) = Right $ TokDeflib p
tokenize ("except", p)         = Right $ TokExcept p
tokenize ("if", p)             = Right $ TokIf p
tokenize ("import", p)         = Right $ TokImport p
tokenize ("lambda", p)         = Right $ TokLambda p
tokenize ("let", p)            = Right $ TokLet p
tokenize ("only", p)           = Right $ TokOnly p
tokenize ("prefix", p)         = Right $ TokPrefix p
tokenize ("quote", p)          = Right $ TokQuote p
tokenize ("rename", p)         = Right $ TokRename p
tokenize ("set!", p)           = Right $ TokSet p

-- Literals

tokenize ("#t", p)             = Right $ TokBool True p
tokenize ("#f", p)             = Right $ TokBool False p
tokenize (s, p)
    | s =~ integer             = Right $ TokInt (read s :: Word64) p
    | s =~ string              = Right $ TokStr (read s :: String) p
    | s =~ identifier          = Right $ TokId s p
    | otherwise                = Left  $ Error ("Unknown token " ++ s) p

-- Check if the pairs of parenthesis are correct

syntaxChecker :: [Token] -> Pos -> Int -> Maybe [Error]

syntaxChecker [] _ 0 = Nothing

syntaxChecker [] p _ =
    Just [Error "Unclosed parenthesis at end of input" p]

syntaxChecker (TokOpen p : r) _ c = syntaxChecker r p (c + 1)

syntaxChecker (TokClose p : r) lp c
    | c > 0 = syntaxChecker r lp (c - 1)
    | otherwise = Just [Error "Unexpected ')'" p]

syntaxChecker (_ : r) p count = syntaxChecker r p count

-- entry point for the lexer

tokenizeList :: [(String, Pos)] -> Either [Error] [Token]
tokenizeList [] = Right []
tokenizeList (h:t) =
    case tokenize h of
        Right tok ->
            case tokenizeList t of
                Right toks -> Right $ tok : toks
                Left errs  -> Left errs
        Left err ->
            case tokenizeList t of
                Right _   -> Left [err]
                Left errs -> Left $ err : errs

lexer :: Program -> Either [Error] [Token]
lexer prog =
    let toks = cut prog in
    tokenizeList toks >>= \t ->
        case syntaxChecker t (Pos 0 0 "") 0 of
            Nothing -> Right t
            Just errs -> Left errs
