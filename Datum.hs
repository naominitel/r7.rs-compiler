module Datum 
(
    Datum(SimpleDatum, ListDatum),
    getDatum
) where

import Constant
import Lexer
import Tree

data Datum 
    = SimpleDatum Constant
    | ListDatum [Datum]
    -- | VectorDatum ???

instance Show Datum where
    show (SimpleDatum constant) = show constant
    show (ListDatum dati) = show dati

getDatum :: TokenTree -> Datum

getDatum (TokLeaf (TokBool b p)) = SimpleDatum $ BoolConstant b p
getDatum (TokLeaf (TokInt i p))  = SimpleDatum $ IntConstant i p
getDatum (TokLeaf tok)  = SimpleDatum $ Symbol (show tok) (tokPos tok)
getDatum (TokNode node) = ListDatum $ map getDatum node
