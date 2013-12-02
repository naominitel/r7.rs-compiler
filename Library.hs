module Library
(
    Library(Library),
    libAddDef,
    libAddExpr,
    libAddImp
) where

import AST
import Compiler
import Define
import Imports

data CmdOrDef = Cmd Expression | Def Define
data Library = Library LibName [Imports] [CmdOrDef]

libAddDef :: Library -> Define -> Library
libAddDef (Library name i defs) d = Library name i $ (Def d) : defs

libAddExpr :: Library -> Expression -> Library
libAddExpr (Library name i exprs) e = Library name i $ (Cmd e) : exprs

libAddImp :: Library -> Imports -> Library
libAddImp (Library name imps cmds) i = Library name (i : imps) cmds

instance Show CmdOrDef where
    show (Cmd e) = show e
    show (Def d) = show d

instance Expand CmdOrDef where
    expand ctx (Cmd e) = Cmd $ expand ctx e
    expand ctx (Def d) = Def $ expand ctx d

instance Show Library where
    show (Library n i a) = "(define-library " ++ show n ++ " " ++ show a

instance Expand Library where
    expand ctxt (Library n i sc) = Library n i $ map (expand ctxt) sc
