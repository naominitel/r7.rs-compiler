module Error
(
    Error(Error),
    Pos(Pos),
    reportErrors,
    reportError,
) where

import System.Console.ANSI
import System.IO

-- Pos: position of a token in the file (Line, Column, Filename)
data Pos = Pos Int Int String

instance Show Pos where
    show (Pos l c f) = f ++ ":" ++ (show l) ++ ":" ++ (show c) ++ ""

-- Error <reason> <where>
data Error = Error String Pos

hGetNLine :: String -> Int -> IO String
hGetNLine fp n = do
    h <- openFile fp ReadMode
    let iter n = do
        if n == 1
            then do
                line <- hGetLine h
                hClose h
                return line
            else do
                _ <- hGetLine h
                iter $ n - 1
    iter n 

reportError :: Error -> IO ()
reportError (Error reason pos@(Pos l c f)) = do
    putStr $ (show pos) ++ ": "

    setSGR [SetColor Foreground Dull Red]
    putStr "error: "
    setSGR [SetColor Foreground Dull White]
    putStrLn reason
    setSGR [Reset]

    line <- hGetNLine f l
    let prfx = f ++ ":" ++ (show l) ++ " "
    let spCount = (length prfx) + (c - 1)
    putStrLn $ prfx ++ line

    setSGR [SetColor Foreground Dull Red]
    putStrLn $ (take spCount $ repeat ' ') ++ "^~"
    setSGR [Reset]

reportErrors :: [Error] -> IO ()
reportErrors [] = return ()
reportErrors (err : r) = do
    reportError err
    reportErrors r
    fail "Aborting due to previous errors"
