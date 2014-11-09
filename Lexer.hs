module Lexer
(

    Token
    (
        Begin,
        Bool,
        Close,
        Define,
        Deflib,
        Dot,
        Except,
        Id,
        If,
        Import,
        Int,
        Lambda,
        Let,
        Only,
        Open,
        Prefix,
        Quote,
        Rename,
        Set,
        Str
    ),

    lexer,
    program,
    syntaxChecker
) where

import Data.Word
import Text.Regex.Posix

import Error

-- Token: basic data structure for a program lexical unit.
--  * Open is an opening brace '('
--  * Close is a closing brace ')'
--  * others are any other identifier or token

data Token
    = Begin
    | Bool Bool
    | Close
    | Define
    | Deflib
    | Dot
    | Except
    | Id String
    | If
    | Int Word64
    | Import
    | Lambda
    | Let
    | Only
    | Open
    | Prefix
    | Quote
    | Rename
    | Set
    | Str String

instance Show Token where
    show Begin      = "begin"
    show Define     = "define"
    show Deflib     = "define-library"
    show Except     = "except"
    show If         = "if"
    show Import     = "import"
    show Lambda     = "lambda"
    show Let        = "let"
    show Only       = "only"
    show Prefix     = "prefix"
    show Quote      = "quote"
    show Rename     = "rename"
    show Set        = "set!"
    show Dot        = "."
    show Open       = "("
    show Close      = ")"
    show (Bool b)   = show b
    show (Int i)    = show i
    show (Str s)    = show s
    show (Id t)     = t

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

tokenize :: String -> Maybe Token

-- Special tokens

tokenize "."                  = Just $ Dot
tokenize "("                  = Just $ Open
tokenize ")"                  = Just $ Close

-- Keywords

tokenize "begin"              = Just $ Begin
tokenize "define"             = Just $ Define
tokenize "define-library"     = Just $ Deflib
tokenize "except"             = Just $ Except
tokenize "if"                 = Just $ If
tokenize "import"             = Just $ Import
tokenize "lambda"             = Just $ Lambda
tokenize "let"                = Just $ Let
tokenize "only"               = Just $ Only
tokenize "prefix"             = Just $ Prefix
tokenize "quote"              = Just $ Quote
tokenize "rename"             = Just $ Rename
tokenize "set!"               = Just $ Set

-- Literals

tokenize "#t"                 = Just $ Bool True
tokenize "#f"                 = Just $ Bool False
tokenize s
    | s =~ integer            = Just $ Int (read s :: Word64)
    | s =~ string             = Just $ Str (read s :: String)
    | s =~ identifier         = Just $ Id s
    | otherwise               = Nothing

-- Check if the pairs of parenthesis are correct

syntaxChecker :: [(Token, Pos)] -> Pos -> Int -> Maybe [Error]
syntaxChecker [] _ 0 = Nothing
syntaxChecker [] p _ = Just [Error "Unclosed parenthesis at end of input" p]
syntaxChecker ((Open, p) : r) _ c = syntaxChecker r p (c + 1)
syntaxChecker ((Close, p) : r) lp c
    | c > 0 = syntaxChecker r lp (c - 1)
    | otherwise = Just [Error "Unexpected ')'" p]
syntaxChecker (_ : r) p count = syntaxChecker r p count

-- entry point for the lexer

tokenizeList :: [(String, Pos)] -> Either [Error] [(Token, Pos)]
tokenizeList [] = Right []
tokenizeList ((h, pos) : t) =
    case tokenize h of
        Just tok ->
            case tokenizeList t of
                Right toks -> Right $ (tok, pos) : toks
                Left errs  -> Left errs
        Nothing ->
            let err = Error ("Unknown token " ++ h) pos in
            case tokenizeList t of
                Right _   -> Left [err]
                Left errs -> Left $ err : errs

lexer :: Program -> Either [Error] [(Token, Pos)]
lexer prog =
    let toks = cut prog in
    tokenizeList toks >>= \t ->
        case syntaxChecker t (Pos 0 0 "") 0 of
            Nothing -> Right t
            Just errs -> Left errs
