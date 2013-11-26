module Library
(
    Library(Library),
    LibName
) where

import AST
import Compiler
import Define

type LibName = [String]
data Library = Library LibName Scope

instance Show Library where
    show (Library n a) = "(define-library " ++ show n ++ " " ++ show a

instance Expression Library where
    codegen (Library _ _) =
        fail "Unimplemented"
