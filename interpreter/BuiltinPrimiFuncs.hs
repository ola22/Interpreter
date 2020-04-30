module BuiltinPrimiFuncs where

import Definitions
import TreeEvaluator








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

{-}
haskellIf :: [Data] -> Data 
haskellIf [DError e, ~_, ~_] = DError e
haskellIf [_, ~(DError e), ~_] = DError e
haskellIf [_, ~_, ~(DError e)] = DError e
haskellIf [DBool b, e1, e2] = if b then e1 else e2
    --if (ifCheckIfExpTypesMatches e1 e2)
        --then if b then e1 else e2
        --else DError "Types mismatch in if branches"
haskellIf _ = DError "Given not a boolean expression in if statement"

ifCheckIfExpTypesMatches :: Data -> Data -> Bool
ifCheckIfExpTypesMatches ~(DInt _) ~(DInt _) = True
ifCheckIfExpTypesMatches ~(DBool _) ~(DBool _) = True
ifCheckIfExpTypesMatches ~(DFunc _ _ _) ~(DFunc _ _ _) = True
ifCheckIfExpTypesMatches ~(DPrimi _) ~(DPrimi _) = True
ifCheckIfExpTypesMatches _ _ = False
-}




----------------------------------------- PrimitiveFuncs for list operations ---------------------------------

primiEmpty :: PrimitiveListFunc
primiEmpty = PrimitiveListFunc 0 haskellEmpty

haskellEmpty :: Env -> [Data] -> Data 
haskellEmpty _ _ = DEvaluatedList []



primiIsEmpty :: PrimitiveListFunc
primiIsEmpty = PrimitiveListFunc 1 haskellIsEmpty

haskellIsEmpty :: Env -> [Data] -> Data
haskellIsEmpty _ [DError err] = DError (err ++ ". Error inside list")
haskellIsEmpty _ [DList l] = DBool (null l)
haskellIsEmpty _ [DEvaluatedList l] = DBool (null l)
haskellIsEmpty _ _ = DError "Trying to apply 'isEmpty' to not-list object"



primiHead :: PrimitiveListFunc
primiHead = PrimitiveListFunc 1 haskellHead

haskellHead :: Env -> [Data] -> Data
haskellHead _ [DError err] = DError (err ++ ". Error inside list")
haskellHead env [DList l] = 
    case l of
        [] -> DError "Trying to apply 'head' to empty list"
        _ -> evaluateTree env (head l)
haskellHead _ [DEvaluatedList l] =
    case l of
        [] -> DError "Trying to apply 'head' to empty list"
        _ -> head l
haskellHead _ _ = DError "Trying to apply 'head' to not-list object"



primiTail :: PrimitiveListFunc
primiTail = PrimitiveListFunc 1 haskellTail

haskellTail :: Env -> [Data] -> Data 
haskellTail _ [DError err] = DError (err ++ ". Error inside list")
haskellTail env [DList l] =
    case l of
        [] -> DError "Trying to apply 'tail' to empty list"
        _ -> evaluateTree env (TData (DList (tail l)))
haskellTail _ [DEvaluatedList l] =
    case l of
        [] -> DError "Trying to apply 'tail' to empty list"
        _ -> DEvaluatedList (tail l)
haskellTail _ _ = DError "Trying to apply 'tail' to not-list object"



primiConcat :: PrimitiveListFunc
primiConcat = PrimitiveListFunc 2 haskellConcat

haskellConcat :: Env -> [Data] -> Data 
haskellConcat _ (DError err:_) = DError (err ++ ". Error inside first list")
haskellConcat _ (_:[DError err]) = DError (err ++ ". Error inside second list")
haskellConcat env (DList l1:[DList l2]) =
    let l1_ = evaluateTree env (TData (DList l1))
        l2_ = evaluateTree env (TData (DList l2))
    in case l1_ of
        DEvaluatedList l1E -> 
            case l2_ of
                DEvaluatedList l2E -> DEvaluatedList (l1E ++ l2E)
                _ -> DError "Trying to apply 'concat' to not-list object"
        _ -> DError "Trying to apply 'concat' to not-list object"
haskellConcat _ (DEvaluatedList l1:[DEvaluatedList l2]) = 
    DEvaluatedList (l1 ++ l2)
haskellConcat _ _ = DError "Trying to apply 'concat' to not-list object"


