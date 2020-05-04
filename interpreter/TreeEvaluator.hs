{-# Options -Wall -Wname-shadowing #-}


module TreeEvaluator where

import qualified Data.Map.Lazy as M


import Definitions
import Printer





-- Function interprets and executes given ParseTree.
evaluateTree :: Env -> ParseTree -> Data
evaluateTree env (TData pos tdata) = 
    case tdata of
        DList l -> 
            let evalList = map (evaluateTree env) l
            in checkList evalList evalList
        DPrimi (PrimitiveFunc _ _ 0 f) -> 
            let res = f []
            in case res of
                (DError err) -> DError ((addPosToError pos) ++ err)
                _ -> res
        _ -> tdata
evaluateTree env (TVar pos var) =
    let maybeVar = M.lookup var env
    in case maybeVar of
            Nothing -> DError ((addPosToError pos) 
                    ++ "Used unknown identifier: " ++ var)
            Just res -> res
evaluateTree env (TFunc _ var_name tree) = DFunc var_name tree env
evaluateTree env (TFAppl pos f v) = 
    let func = evaluateTree env f -- Dfunc/DPrimi
        var_val = evaluateTree env v -- DInt/DBool
    in case func of
            DPrimi p -> evaluatePrimi p pos var_val
            DFunc var_name ftree fenv ->
                evaluateTree (M.insert var_name var_val fenv) ftree
            DError err -> DError err
            _ -> DError ((addPosToError pos) 
                    ++ "Trying to apply argument to non-function object")



-- Function checks if given list contains any errors after evaluation.
-- If yes, then it returns error.
checkList :: [Data] -> [Data] -> Data
checkList l [] = DEvaluatedList l
checkList _ (DError err:_) = DError err
checkList l (_:rest) = checkList l rest



-- Functions call given primitive function or collect next arguments from
-- parse tree
evaluatePrimi :: PrimitiveFunc -> FilePosition -> Data -> Data
evaluatePrimi (PrimitiveFunc _ _ 1 func) pos var = 
    let res = func [var]
    in case res of
        (DError err) -> DError ((addPosToError pos) ++ err)
        _ -> res
evaluatePrimi (PrimitiveFunc name typ n func) _ var = 
    DPrimi (PrimitiveFunc name typ (n-1) (\x -> func (var:x)))


