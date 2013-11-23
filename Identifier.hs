module Identifier
(
    Identifier(Identifier)
) where

import Bytecode
import Compiler
import Data.Word
import Error
import Lexer

-- An identifier, such as "var"

data Identifier = Identifier String Pos

instance Show Identifier where
    show (Identifier var _) = var ++ ":id"

-- Assembly code generation for accessing to a variable
 
instance Expression Identifier where
    codegen (Identifier var p) st@(env, lbl) _ =
        case envFetch var env of 
            (Just envId) -> 
                Pass [Fetch (fromIntegral envId :: Word64)] st
            (Nothing) -> 
                Failure [Error ("Unbound variable " ++ var) p] st
