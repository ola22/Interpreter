{-# Options -Wall -Wname-shadowing #-}


module TreeEvaluator where

import qualified Data.Map.Lazy as M

import Definitions





-- Function interprets and executes given ParseTree.
evaluateTree :: Env -> ParseTree -> Data
evaluateTree env (TData tdata) = 
    case tdata of
        DList l -> 
            let evalList = map (evaluateTree env) l
            in checkList evalList evalList
        DListPrimi (PrimitiveListFunc _ _ 0 f) -> f env []
        _ -> tdata
evaluateTree env (TVar var) =
    let maybeVar = M.lookup var env
    in case maybeVar of
            Nothing -> DError ("Used unknown identifier: " ++ var)
            Just res -> res
evaluateTree env (TFunc var_name tree) = DFunc var_name tree env
evaluateTree env (TFAppl f v) = 
    let func = evaluateTree env f -- Dfunc/DPrimi
        var_val = evaluateTree env v -- DInt/DBool
    in case func of
            DPrimi p -> evaluatePrimi p var_val
            DListPrimi p -> evaluateListPrimi p var_val env
            DFunc var_name ftree fenv ->
                evaluateTree (M.insert var_name var_val fenv) ftree
            DError err -> DError err
            _ -> DError ("Trying to apply argument to non-function object")



-- Function checks if given list contains any errors after evaluation.
-- If yes, then it returns error.
checkList :: [Data] -> [Data] -> Data
checkList l [] = DEvaluatedList l
checkList _ (DError err:_) = DError err
checkList l (_:rest) = checkList l rest



-- Functions call given primitive function or collect next arguments from
-- parse tree
evaluatePrimi :: PrimitiveFunc -> Data -> Data
evaluatePrimi (PrimitiveFunc _ _ 1 func) var = func [var]
evaluatePrimi (PrimitiveFunc name typ n func) var = 
    DPrimi (PrimitiveFunc name typ (n-1) (\x -> func (var:x)))

evaluateListPrimi :: PrimitiveListFunc -> Data -> Env -> Data
evaluateListPrimi (PrimitiveListFunc _ _ 1 func) var env = func env [var]
evaluateListPrimi (PrimitiveListFunc name typ n func) var env = 
    DListPrimi (PrimitiveListFunc name typ (n-1) (\_ y -> func env (var:y)))



