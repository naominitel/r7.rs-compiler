module Test where

import AST
import Eval
import Lexer
import Parser

test :: String -> IO ()
test str = 
    let prog  = program str
        l     = lexer prog
        check = syntaxChecker l 0
    in case check of 
           Left str -> putStrLn $ "syntax error: " ++ str
           Right l -> let p = parser l
                      in case p of              
                           Right ast -> putStrLn $ show $ eval ast []
                           Left str -> putStrLn $ "parse error: " ++ str
                          