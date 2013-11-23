module Error
(
    Error(Error),
    reportError
) where

import Lexer
import System.Console.ANSI
import System.IO

-- Error <reason> <where>
data Error = Error String Pos

hGetNLine :: String -> Int -> IO String
hGetNLine fp n = do
    h <- openFile fp ReadMode
    let iter n = if n == 1 then hGetLine h else let _ = hGetLine h in iter $ n - 1
    iter n 

reportError :: Error -> IO ()
reportError (Error reason pos@(Pos l c f)) = do
    let prfx = f ++ ":" ++ (show l) ++ " "
    putStr $ (show pos) ++ ": "
    setSGR [SetColor Foreground Dull Red]
    putStr "error: "
    setSGR [SetColor Foreground Dull White]
    putStrLn reason
    setSGR [Reset]
    line <- hGetNLine f l
    putStrLn $ prfx ++ line
    let spCount = (length prfx) + (c - 1)
    setSGR [SetColor Foreground Dull Red]
    putStrLn $ (take spCount $ repeat ' ') ++ "^~"
    setSGR [Reset]
    fail "Aborting due to previous errors"
