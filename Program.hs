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
import Bytecode
import Compiler
import Define
import Imports
import Result

data CmdOrDef = Cmd Expression | Def Define
data Program = Program [Imports] [CmdOrDef]

importedLibs :: Program -> [LibName]
importedLibs (Program imps _) =
    let aux imp = case imp of
            Only i _   -> aux i
            Except i _ -> aux i
            Prefix i _ -> aux i
            Rename i _ -> aux i
            Lib lname  -> [lname]
    in concatMap aux (concat imps)

progAddDef :: Program -> Define -> Program
progAddDef (Program i defs) d = Program i (Def d : defs)

progAddExpr :: Program -> Expression -> Program
progAddExpr (Program i defs) e = Program i (Cmd e : defs)

progAddImp :: Program -> Imports -> Program
progAddImp (Program imps d) i = Program (i : imps) d

getDefines :: [CmdOrDef] -> [Define]
getDefines [] = []
getDefines (Def d : r) = d : getDefines r
getDefines (Cmd _ : r) = getDefines r

getCommands :: [CmdOrDef] -> [Expression]
getCommands [] = []
getCommands (Def _ : r) = getCommands r
getCommands (Cmd c : r) = c : getCommands r

-- The compilation of a program body is similar to the compilation of a scope
-- body, except that defines and commands may be interleaved. We first allocate
-- the environment for containing all the defines, then compile each (define)
-- as a store and each command in the environment containg the defines.
-- Every access to a variable defined by one of the (define) before it will
-- cause a runtime error, whereas in the case of a scope body, it is a parse
-- error

compileBody :: [CmdOrDef] -> Word64 -> State -> Compiler.Result
compileBody [] _ st = return st
compileBody (Cmd (Expr a) : r) i st = do
    cmd <- codegen a st False
    cmd <- Pass [Pop] cmd
    compileBody r i cmd
compileBody (Def (Define _ (Expr e) _) : r) i st = do
    def <- codegen e st False
    def <- Pass [Store i] def
    compileBody r (i + 1) def

compileProgram :: Program -> Compiler.Result
compileProgram (Program _ defs) =
    let defines = getDefines defs
        envsize = fromIntegral (length defines) :: Word64
        env     = map (\(Define var _ _) -> var) defines
        initst  = envExtend env initialState
    in Pass [Alloc envsize] initst >>= compileBody defs 0

instance Show CmdOrDef where
    show (Cmd c) = show c
    show (Def d) = show d

instance Expand CmdOrDef where
    expand ctx (Def d) = Def $ expand ctx d
    expand ctx (Cmd c) = Cmd $ expand ctx c

instance Show Program where
    show (Program _ cmds) = show cmds

instance Expand Program where
    expand ctx (Program i cmds) = Program i $ map (expand ctx) cmds
