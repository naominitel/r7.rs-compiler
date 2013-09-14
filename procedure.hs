module Procedure
(
    Procedure(Token, Pos)
) where

-- a procedure is an evaluator-builtin function

data Procedure = Procedure Token Pos

instance show Procedure where
    show (Procedure t _) = show t