{-# Options -Wall -Wname-shadowing #-}

module TreeEvaluator where

import qualified Data.Map.Lazy as M
import Definitions

--import Debug.Trace


-- zmienic primitive
-- zmienic Show
-- zmienic testy
-- dopisac testy
-- data???
-- zmienic evaluateTree??-- co interpreter a co evaluateTree???????
-- tego evala jakos inaczej :o

-- te całe primi na coś mniej trickowego
-- lambdy też są jakieś trickowe

-- czy nie wole najpierw sprawdzic case var_val

-- SPRAWDZIC,CZY E2 JEST TYPU E1 W IFIE




-- Function interprets and executes given ParseTree.
evaluateTree :: ParseTree -> Env -> Data
evaluateTree (TData tdata) _ = 
    case tdata of
        --DList l -> undefined         -----> eval pokolei na kazdym elemencie listy
        _ -> tdata
evaluateTree (TVar var) env =
    let maybeVar = M.lookup var env
    in case maybeVar of
            Nothing -> DError ("Used invalid identifier: " ++ var)
            Just res -> res
evaluateTree (TFunc var_name tree) env = DFunc var_name tree env
evaluateTree (TFAppl f v) env = 
    let func = evaluateTree f env -- :: Dfunc/DPrimi
        var_val = evaluateTree v env -- :: DInt/DBool
    in case func of
            DPrimi p -> evaluatePrimi p var_val
            DListPrimi p -> evaluateListPrimi p var_val env
            DFunc var_name ftree fenv ->
                evaluateTree ftree (M.insert var_name var_val fenv)
            DError err -> DError err
            _ -> DError ("Trying to apply to sth, that is not a proper function")



-- prymitywki :o <3
evaluatePrimi :: PrimitiveFunc -> Data -> Data
evaluatePrimi (PrimitiveFunc 1 func) var = func [var]
evaluatePrimi (PrimitiveFunc n func) var = DPrimi (PrimitiveFunc (n-1) (\x -> func (var:x)))


-- prymitywki :o <3
evaluateListPrimi :: PrimitiveListFunc -> Data -> Env -> Data
evaluateListPrimi (PrimitiveListFunc 0 func) _ env = func env []
evaluateListPrimi (PrimitiveListFunc 1 func) var env = func env [var]
evaluateListPrimi (PrimitiveListFunc _ _) _ _ = DError "List primitive got too many arguments"




----------------------------------------- PrimitiveFunc arithmetical operations ---------------------------------


primiAdd :: PrimitiveFunc
primiAdd = PrimitiveFunc 2 haskellAdd

haskellAdd :: [Data] -> Data
haskellAdd ((DInt x):[DInt y]) = DInt (x + y)
haskellAdd (DError e:_) = DError e
haskellAdd (_:[DError e]) = DError e
haskellAdd _ = DError $ "Given argument of wrong type " ++
                     "to '+' operator. Expected type: Int"


primiSub :: PrimitiveFunc
primiSub = PrimitiveFunc 2 haskellSub

haskellSub :: [Data] -> Data
haskellSub ((DInt x):[DInt y]) = DInt (x - y)
haskellSub (DError e:_) = DError e
haskellSub (_:[DError e]) = DError e
haskellSub _ = DError $ "Given argument of wrong type " ++
                     "to '-' operator. Expected type: Int"


primiMul :: PrimitiveFunc
primiMul = PrimitiveFunc 2 haskellMul

haskellMul :: [Data] -> Data
haskellMul ((DInt x):[DInt y]) = DInt (x * y)
haskellMul (DError e:_) = DError e
haskellMul (_:[DError e]) = DError e
haskellMul _ = DError $ "Given argument of wrong type " ++
                     "to '*' operator. Expected type: Int"


primiDiv :: PrimitiveFunc
primiDiv = PrimitiveFunc 2 haskellDiv

haskellDiv :: [Data] -> Data
haskellDiv (_:[DInt 0]) = DError "Divide by 0"
haskellDiv ((DInt x):[DInt y]) = DInt (x `div` y)
haskellDiv (DError e:_) = DError e
haskellDiv (_:[DError e]) = DError e
haskellDiv _ = DError $ "Given argument of wrong type " ++
                     "to '/' operator. Expected type: Int"


primiMod :: PrimitiveFunc
primiMod = PrimitiveFunc 2 haskellMod

haskellMod :: [Data] -> Data
haskellMod ((DInt x):[DInt y]) = DInt (x `mod` y)
haskellMod (DError e:_) = DError e
haskellMod (_:[DError e]) = DError e
haskellMod _ = DError $ "Given argument of wrong type " ++
                     "to '%' operator. Expected type: Int"



----------------------------------------- PrimitiveFunc logical operatorations ---------------------------------

primiAnd :: PrimitiveFunc
primiAnd = PrimitiveFunc 2 haskellAnd

haskellAnd :: [Data] -> Data
haskellAnd ((DBool b1):[DBool b2]) = DBool (b1 && b2)
haskellAnd (DError e:_) = DError e
haskellAnd (_:[DError e]) = DError e
haskellAnd _ = DError $ "Given argument of wrong type " ++
                     "to '&&' operator. Expected type: Bool"


primiOr :: PrimitiveFunc
primiOr = PrimitiveFunc 2 haskellOr

haskellOr :: [Data] -> Data
haskellOr ((DBool b1):[DBool b2]) = DBool (b1 || b2)
haskellOr (DError e:_) = DError e
haskellOr (_:[DError e]) = DError e
haskellOr _ = DError $ "Given argument of wrong type " ++
                     "to '||' operator. Expected type: Bool"


----------------------------------------- PrimitiveFunc comparison operations ---------------------------------

primiEq :: PrimitiveFunc
primiEq = PrimitiveFunc 2 haskellEq

haskellEq :: [Data] -> Data
haskellEq ((DInt x):[DInt y]) = DBool (x == y)
haskellEq (DError e:_) = DError e
haskellEq (_:[DError e]) = DError e
haskellEq _ = DError $ "Given argument of wrong type " ++
                     "to '==' operator. Expected type: Int"


primiNotEq :: PrimitiveFunc
primiNotEq = PrimitiveFunc 2 haskellNotEq

haskellNotEq :: [Data] -> Data
haskellNotEq ((DInt x):[DInt y]) = DBool (not (x == y))
haskellNotEq (DError e:_) = DError e
haskellNotEq (_:[DError e]) = DError e
haskellNotEq _ = DError $ "Given argument of wrong type " ++
                     "to '!=' operator. Expected type: Int"


primiLess :: PrimitiveFunc
primiLess = PrimitiveFunc 2 haskellLess

haskellLess :: [Data] -> Data
haskellLess ((DInt x):[DInt y]) = DBool (x < y)
haskellLess (DError e:_) = DError e
haskellLess (_:[DError e]) = DError e
haskellLess _ = DError $ "Given argument of wrong type " ++
                     "to '<' operator. Expected type: Int"


primiMore :: PrimitiveFunc
primiMore = PrimitiveFunc 2 haskellMore

haskellMore :: [Data] -> Data
haskellMore ((DInt x):[DInt y]) = DBool (x > y)
haskellMore (DError e:_) = DError e
haskellMore (_:[DError e]) = DError e
haskellMore _ = DError $ "Given argument of wrong type " ++
                     "to '>' operator. Expected type: Int"


------------------------------------------- If statement ----------------------------------------

primiIf :: PrimitiveFunc
primiIf = PrimitiveFunc 3 haskellIf

haskellIf :: [Data] -> Data 
haskellIf [DError e, _, _] = DError e
haskellIf [DBool b, e1, e2] = if b then e1 else e2
haskellIf _ = DError "Given not a boolean expression in if statement"

{-
haskellIf [DError e, _, _] = DError e
haskellIf [_, DError e, _] = DError e
haskellIf [_, _, DError e] = DError e
haskellIf [DBool b, e1, e2] = 
    case (ifCheckIfExpTypesMatches e1 e2) of
        True -> if b then e1 else e2
        False -> DError "Types of expressions in 'then else' doesn't match"
haskellIf _ = DError "Given not a boolean expression in if statement"

ifCheckIfExpTypesMatches :: Data -> Data -> Bool
ifCheckIfExpTypesMatches (DInt _) (DInt _) = True
ifCheckIfExpTypesMatches (DBool _) (DBool _) = True
ifCheckIfExpTypesMatches (DFunc _ _ _) (DFunc _ _ _) = True
ifCheckIfExpTypesMatches (DPrimi _) (DPrimi _) = True
ifCheckIfExpTypesMatches _ _ = False
-}



----------------------------------------- PrimitiveFuncs for list operations ---------------------------------

primiEmpty :: PrimitiveListFunc
primiEmpty = PrimitiveListFunc 0 haskellEmpty

haskellEmpty :: Env -> [Data] -> Data 
haskellEmpty _ _ = DEvaluatedList []


primiHead :: PrimitiveListFunc
primiHead = PrimitiveListFunc 1 haskellHead

haskellHead :: Env -> [Data] -> Data 
haskellHead env [DList l] = evaluateTree (head l) env
haskellHead _ _ = DError "Trying to apply 'head' to not-list object"


primiTail :: PrimitiveListFunc
primiTail = PrimitiveListFunc 1 haskellTail

haskellTail :: Env -> [Data] -> Data 
haskellTail env [DList l] = evaluateTree (TData (DList (tail l))) env
haskellTail _ _ = DError "Trying to apply 'tail' to not-list object"






{- | Basic evaluateTree tests
>>> evaluateTree ( TData $ DInt 5 ) M.empty
DInt 5
>>> evaluateTree ( TData $ DBool True ) M.empty
DBool True

>>> evaluateTree ( TData $ DInt 5 ) M.empty
DInt 5

>>> evaluateTree (TFAppl (TData (DFunc "x" ( TFAppl ( TFAppl (TData $ DPrimi primiMul) (TVar "x") ) (TVar "x")) M.empty)) (TData $ DInt 5)) M.empty
DInt 25
-}


{- | Primitives
>>> evaluateTree ( TFAppl ( TFAppl (TData $DPrimi primiAdd) (TData $ DInt 2) ) (TData $ DInt 2)) M.empty
DInt 4
>>> evaluateTree ( TFAppl ( TFAppl (TData $DPrimi primiSub) (TData $ DInt 5) ) (TData $ DInt 2)) M.empty
DInt 3
>>> evaluateTree ( TFAppl ( TFAppl (TData $DPrimi primiMul) (TData $ DInt 5) ) (TData $ DInt 2)) M.empty
DInt 10
>>> evaluateTree ( TFAppl ( TFAppl (TData $DPrimi primiDiv) (TData $ DInt 10) ) (TData $ DInt 3)) M.empty
DInt 3
>>> evaluateTree ( TFAppl ( TFAppl (TData $DPrimi primiMod) (TData $ DInt 10) ) (TData $ DInt 3)) M.empty
DInt 1
>>> evaluateTree ( TFAppl ( TFAppl (TData $DPrimi primiAnd) (TData $ DBool True) ) (TData $ DBool True)) M.empty
DBool True
-}