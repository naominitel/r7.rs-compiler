module Library
(
    Library(Library),
    LibName
) where

import AST
import Define

type LibName = [String]
data Library = Library LibName Scope

instance Show Library where
    show (Library n a) = "(define-library " ++ show n ++ " " ++ show a

instance Expand Library where
    expand ctxt (Library n sc) = Library n $ expand ctxt sc
