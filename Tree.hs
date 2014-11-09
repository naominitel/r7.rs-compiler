module Tree
(
    TokenTree (Leaf, Node),
    first,
    ttPos,
) where

import Error (Pos)
import Lexer (Token)

-- Basic representation of the program after first phase of parsing
-- Tree where leaf/nodes are tokens with their contents and positions attached

data TokenTree
    = Leaf Token Pos
    | Node [TokenTree] Pos

ttPos (Leaf _ p) = p
ttPos (Node _ p) = p

instance Show TokenTree where
    show (Leaf tok _) = "Leaf: " ++ show tok
    show (Node lst _) = "Node: " ++ show lst

-- returns the first token of the tree as a string, to be displayed to
-- the user when this tree if found in an errorneous position while parsing

first (Leaf t _) = show t
first (Node _ _) = "("
