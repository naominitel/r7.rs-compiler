{-# LANGUAGE ExistentialQuantification #-}
module AST
(
    AST(AST),
    Expand(expand)
) where

-- The AST type represent any syntax tree after first parsing phase but that
-- may be expanded or not.
-- Every AST must implement the function expand use by the macro expanser that
-- takes the expansion context and return the expanded tree

data AST = forall a. Expand a => AST a

-- FIXME: replace this placeholder expansion context by something more useful
type Context = [String]

class (Show a) => Expand a where
    expand :: Context -> a -> a

instance Show AST where
    show (AST a) = show a

instance Expand AST where
    expand ctxt (AST a) = AST $ expand ctxt a
