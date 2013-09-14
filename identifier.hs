module Identifier
(
    Identifier(Identifier)
) where

import Bytecode
import Compiler
import Data.Word
import Lexer

-- An identifier, such as "var"

data Identifier = Identifier String Pos

instance Show Identifier where
    show (Identifier var _) = var ++ ":id"

-- Assembly code generation for accessing to a variable
 
instance Expression Identifier where
    codegen (Identifier var p) st@(env, lbl) = 
        case (primFetch var, envFetch var env) of 
            (Just primId, _) -> 
                return ([Push (TPrim (fromIntegral primId :: Word32))], st)
            (_, Just envId) -> 
                return ([Fetch (fromIntegral envId :: Word32)], st)
            (Nothing, Nothing) -> 
                fail $ "Unbound variable " ++ var ++ " at " ++ show p