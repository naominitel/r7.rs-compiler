module Program
(
    Program(Program),
    CmdOrDef(Cmd, Def),
    compileProgram,
    importedLibs,
    progAddDef,
    progAddExpr,
    progAddImp
) where

import Data.Word

import AST
import Begin
import Bytecode
import Compiler
import Define
import Error
import Imports
import Library

data CmdOrDef = Cmd AST | Def Define
data Program = Program [Imports] [CmdOrDef]

importedLibs :: Program -> [LibName]
importedLibs (Program imps _) =
    let aux = \imp ->
            case imp of
                Only i _ -> aux i
                Except i _ -> aux i
                Prefix i _ -> aux i
                Rename i _ -> aux i
                Lib lname -> [lname]
    in concat $ map aux $ concat imps


progAddDef :: Program -> Define -> Program
progAddDef (Program i defs) d = Program i (Def d : defs)

progAddExpr :: Program -> AST -> Program
progAddExpr (Program i defs) e = Program i (Cmd e : defs)

progAddImp :: Program -> Imports -> Program
progAddImp (Program imps d) i = Program (i : imps) d

getDefines :: [CmdOrDef] -> [Define]
getDefines [] = []
getDefines (Def d : r) = d : (getDefines r)
getDefines (Cmd c : r) = getDefines r

getCommands :: [CmdOrDef] -> [AST]
getCommands [] = []
getCommands (Def d : r) = getCommands r
getCommands (Cmd c : r) = c : (getCommands r)

-- The compilation of a program body is similar to the compilation of a scope
-- body, except that defines and commands may be interleaved. We first allocate
-- the environment for containing all the defines, then compile each (define)
-- as a store and each command in the environment containg the defines.
-- Every access to a variable defined by one of the (define) before it will
-- cause a runtime error, whereas in the case of a scope body, it is a parse
-- error

compileBody :: [CmdOrDef] -> Word64 -> State -> Result
compileBody [] _ st = Pass [] st
compileBody (Cmd (AST a) : r) i st =
    let rec = codegen a st False in
    continue rec
        (compileBody r i)
        (\irec ir -> irec ++ [(Pop)] ++ ir)
compileBody (Def (Define var (AST e) _) : r) i st =
    let rec = codegen e st False in
    continue rec
        (compileBody r $ i + 1)
        (\irec ir -> irec ++ [(Store i)] ++ ir)

compileProgram :: Program -> Result
compileProgram (Program _ defs) =
    let defines = (getDefines defs)
        envsize = fromIntegral (length defines) :: Word64
        env = map (\(Define var _ _) -> var) defines
        initst = envExtend env initialState
        rec = compileBody defs 0 initst
    in continue rec
        (\st -> Pass [] st)
        (\irec _ -> [Alloc envsize] ++ irec)
