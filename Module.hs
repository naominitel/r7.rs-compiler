module Module
(
    Module(Prog, Lib),
    CompiledModule(Mod),
    compileModule
) where

import Data.Word

import Bytecode
import Compiler
import Error
import Lexer
import Library
import Program

data Module = Prog Program | Lib Library

compileModule :: Module -> Either [Error] CompiledModule
compileModule (Prog p) =
    case compileProgram p of
        Failure errs st -> Left errs
        Pass instrs st -> Right $ Mod 0 (importedLibs p) instrs

compileModule (Lib l) = Left [Error "Unimplemented" (Pos 0 0 "")]

-- A compiled module

data CompiledModule = Mod Word64 [LibName] [Instr]
