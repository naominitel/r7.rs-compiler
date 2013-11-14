
data Datum 
    = SimpleDatum Constant
    | ListDatum [Datum]
    --| VectorDatum ???

getDatum :: TokenTree -> Datum

getDatum (TokLeaf (TokBool b p)) = SimpleDatum $ BoolConstant b p
getDatum (TokLeaf (TokInt i p)) = SimpleDatum $ IntConstant i p

--get