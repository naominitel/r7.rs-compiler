module Eval ( eval ) where

import AST
import Env
import Debug.Trace

-- eval: core evaluator. Returns the value of an expression if it can be
-- computed successfully, otherwise returns an exception value

eval :: AST -> Env -> Either Value Value

-- eval for (lambda (a) e)

eval (Lambda arg expr pos) env =
    return $ Function arg expr

-- eval for function call (expr1 expr2)

eval (Apply f arg pos) env =
    let func = eval f env
    in case func of
        (Left except) -> Left except
        (Right (Function a expr)) -> 
            let val = eval arg env 
            in val >>= \v -> eval expr $ envExtend env a v
        --(Right (Procedure t)) ->
        --    let val = eval arg env 
        --    in val
        _ -> Left $ Exception "Called expression is not a function"

-- eval for if (if e1 e2 e3)

eval (If exprif expr1 expr2 pos) env =
    let valIf = eval exprif env
    in valIf >>= \v -> eval (if v == (Boolean False) then expr2 else expr1) env

-- eval for constants

eval (Constant v pos) env = return v

-- eval for identifier

eval (Identifier i pos) env =
    case envFetch i env of
        Just val -> return val
        Nothing -> Left $ Exception $ "Unbound variable: " ++ i ++ " at " ++ show pos


