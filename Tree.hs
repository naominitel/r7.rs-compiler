module Tree
(
    TokenTree(TokLeaf, TokNode)
) where

import Lexer

-- Basic representation of the program after first phase of parsing
-- Tree where leaf/nodes are tokens with their contents and positions attached

data TokenTree
    = TokLeaf Token
    | TokNode [TokenTree]

instance Show TokenTree where
    show (TokLeaf tok) = "Leaf: " ++ show tok
    show (TokNode lst) = "Node: " ++ show lst
