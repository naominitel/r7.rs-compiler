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
getDatum (Leaf (Bool b) p) = SimpleDatum $ BoolConstant b p
getDatum (Leaf (Int i) p)  = SimpleDatum $ IntConstant i p
getDatum (Leaf tok p)  = SimpleDatum $ Symbol (show tok) p
getDatum (Node node _) = ListDatum $ map getDatum node
