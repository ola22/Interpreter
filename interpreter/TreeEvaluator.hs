{-# Options -Wall -Wname-shadowing #-}

module TreeEvaluator where

import qualified Data.Map.Lazy as M
import Definitions


--import Debug.Trace


-- TODO
-- SPRAWDZIC,CZY E2 JEST TYPU E1 W IFIE




-- Function interprets and executes given ParseTree.
evaluateTree :: Env -> ParseTree -> Data
evaluateTree env (TData tdata) = 
    case tdata of
        DList l -> 
            let evalList = map (evaluateTree env) l
            in checkList evalList evalList
        _ -> tdata
evaluateTree env (TVar var) =
    let maybeVar = M.lookup var env
    in case maybeVar of
            Nothing -> DError ("Used unknown identifier: " ++ var)
            Just res -> res
evaluateTree env (TFunc var_name tree) = DFunc var_name tree env
evaluateTree env (TFAppl f v) = 
    let func = evaluateTree env f -- :: Dfunc/DPrimi
        var_val = evaluateTree env v -- :: DInt/DBool
    in case func of
            DPrimi p -> evaluatePrimi p var_val
            DListPrimi p -> evaluateListPrimi p var_val env
            DFunc var_name ftree fenv ->
                evaluateTree (M.insert var_name var_val fenv) ftree
            DError err -> DError err
            _ -> DError ("Trying to apply argument to non-function object")



-- Function checks if given list contains any errors after evaluation
checkList :: [Data] -> [Data] -> Data
checkList l [] = DEvaluatedList l
checkList _ (DError err:_) = DError err
checkList l (_:rest) = checkList l rest



-- Functions call given primitive function or collect next arguments from
-- parse tree
evaluatePrimi :: PrimitiveFunc -> Data -> Data
evaluatePrimi (PrimitiveFunc 1 func) var = func [var]
evaluatePrimi (PrimitiveFunc n func) var = DPrimi (PrimitiveFunc (n-1) (\x -> func (var:x)))

evaluateListPrimi :: PrimitiveListFunc -> Data -> Env -> Data
evaluateListPrimi (PrimitiveListFunc 0 func) _ env = func env []
evaluateListPrimi (PrimitiveListFunc 1 func) var env = func env [var]
evaluateListPrimi (PrimitiveListFunc n func) var env = 
    DListPrimi (PrimitiveListFunc (n-1) (\_ y -> func env (var:y)))




{- | Basic evaluateTree tests
>>> evaluateTree M.empty ( TData $ DInt 5 )
DInt 5
>>> evaluateTree M.empty ( TData $ DBool True )
DBool True

>>> evaluateTree M.empty ( TData $ DInt 5 )
DInt 5

>>> evaluateTree M.empty (TFAppl (TData (DFunc "x" ( TFAppl ( TFAppl (TData $ DPrimi primiMul) (TVar "x") ) (TVar "x")) M.empty)) (TData $ DInt 5))
DInt 25
-}


{- | Primitives
>>> evaluateTree M.empty ( TFAppl ( TFAppl (TData $DPrimi primiAdd) (TData $ DInt 2) ) (TData $ DInt 2))
DInt 4
>>> evaluateTree M.empty ( TFAppl ( TFAppl (TData $DPrimi primiSub) (TData $ DInt 5) ) (TData $ DInt 2))
DInt 3
>>> evaluateTree M.empty ( TFAppl ( TFAppl (TData $DPrimi primiMul) (TData $ DInt 5) ) (TData $ DInt 2)) 
DInt 10
>>> evaluateTree M.empty ( TFAppl ( TFAppl (TData $DPrimi primiDiv) (TData $ DInt 10) ) (TData $ DInt 3))
DInt 3
>>> evaluateTree M.empty ( TFAppl ( TFAppl (TData $DPrimi primiMod) (TData $ DInt 10) ) (TData $ DInt 3))
DInt 1
>>> evaluateTree M.empty ( TFAppl ( TFAppl (TData $DPrimi primiAnd) (TData $ DBool True) ) (TData $ DBool True))
DBool True
-}