module Apply
(
    Apply(Apply)
) where

import AST
import Error
import Bytecode
import Compiler
import Data.Word
import Lexer

-- a function call (f arg)

data Apply = Apply AST [AST]

instance Show Apply where
    show (Apply func args) =
        let pargs = map show args
        in "(#%app " ++ show func ++ " " ++ show args ++ ")"
 
 -- compile the expressions passed as arguments

compileArgs :: [AST] -> CompilerState -> Either Error ([Instr], CompilerState)
compileArgs [] st = return ([], st)
compileArgs ((AST a) : rest) st =
    let exp1 = codegen a st False
    in exp1 >>= \(exp,st1) -> 
        let rec = compileArgs rest st1
        in rec >>= \(r,st2) -> return (exp ++ r, st2)

-- Assembly generation for function application
-- (f a b c) is compiled the following way :
--   <compilation of c>
--   <compilation of b>
--   <compilation of a>
--   <compilation of the f expression>
--   CALL 3             

instance Expression Apply where
    codegen (Apply (AST f) parms) st pos =
        let args = compileArgs (reverse parms) st
        in args >>= \(a,st1) -> 
            let func = codegen f st1 False
            in func >>= \(f, st2) -> return 
                (a ++ f ++ if pos
                    then [TCall (fromIntegral (length parms) :: Word8)]
                    else [Call (fromIntegral (length parms) :: Word8)],
                st2)
