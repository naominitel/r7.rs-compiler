module Module
(
    Module(Prog, Lib),
    compileModule
) where

import Bytecode
import Compiler
import Error
import Lexer
import Library
import Program

data Module = Prog Program | Lib Library

compileModule :: Module -> Result
compileModule (Prog p) = compileProgram p
compileModule (Lib l) = Failure [Error "Unimplemented" (Pos 0 0 "")] initialState
