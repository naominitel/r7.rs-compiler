module Module
(
    Module(Prog, Lib),
    CompiledModule(Mod),
    compileModule
) where

import Data.Word

import Bytecode
import Error
import Imports
import Library
import Program
import Result

data Module = Prog Program | Lib Library

compileModule :: Module -> Either [Error] CompiledModule
compileModule (Prog p) =
    case compileProgram p of
        Failure errs _ -> Left errs
        Pass instrs _  -> Right $ Mod 0 (importedLibs p) instrs

compileModule (Module.Lib _) = Left [Error "Unimplemented" (Pos 0 0 "")]

-- A compiled module

data CompiledModule = Mod Word64 [LibName] [Instr]
