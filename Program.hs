module Program
(
    Program(Program),
    CmdOrDef(Cmd, Def),
    compileProgram,
    progAddDef,
    progAddExpr,
    progAddImp
) where

import AST
import Begin
import Bytecode
import Compiler
import Define
import Error
import Imports

data CmdOrDef = Cmd AST | Def Define
data Program = Program [Imports] [CmdOrDef]

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

toScope :: Program -> Scope
toScope (Program _ b) =
    let defs = getDefines b
        cmds = getCommands b
    in Scope defs (Begin cmds)

-- FIXME: currently we compile the program as if all the defines
-- were before any of the commandsd
compileProgram :: Program -> Result
compileProgram p =
    let sc = toScope p 
        st = initialState 
    in codegen sc st False
