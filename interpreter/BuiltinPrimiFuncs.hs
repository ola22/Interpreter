{-# Options -Wall -Wname-shadowing #-}

module BuiltinPrimiFuncs where

import Definitions





----------------------------------------- Primitive operations types ------------------------------------
-------------------------------------------- (for type inference) ---------------------------------------

arOpType :: Type
arOpType = TypeFunc TypeInt $ TypeFunc TypeInt TypeInt

comOpType :: Type
comOpType = TypeFunc TypeInt $ TypeFunc TypeInt TypeBool

logOpType :: Type
logOpType = TypeFunc TypeBool $ TypeFunc TypeBool TypeBool




----------------------------------------- PrimitiveFunc arithmetical operations ---------------------------------


primiAdd :: PrimitiveFunc
primiAdd = PrimitiveFunc "Addition" arOpType 2 haskellAdd

haskellAdd :: [Data] -> Data
haskellAdd ((DInt x):[DInt y]) = DInt (x + y)
haskellAdd (DError e:_) = DError e
haskellAdd (_:[DError e]) = DError e
haskellAdd _ = DError $ "Given argument of wrong type " ++
                     "to '+' operator. Expected type: Int"


primiSub :: PrimitiveFunc
primiSub = PrimitiveFunc "Subtraction" arOpType 2 haskellSub

haskellSub :: [Data] -> Data
haskellSub ((DInt x):[DInt y]) = DInt (x - y)
haskellSub (DError e:_) = DError e
haskellSub (_:[DError e]) = DError e
haskellSub _ = DError $ "Given argument of wrong type " ++
                     "to '-' operator. Expected type: Int"


primiMul :: PrimitiveFunc
primiMul = PrimitiveFunc "Multiplication" arOpType 2 haskellMul

haskellMul :: [Data] -> Data
haskellMul ((DInt x):[DInt y]) = DInt (x * y)
haskellMul (DError e:_) = DError e
haskellMul (_:[DError e]) = DError e
haskellMul _ = DError $ "Given argument of wrong type " ++
                     "to '*' operator. Expected type: Int"


primiDiv :: PrimitiveFunc
primiDiv = PrimitiveFunc "Division" arOpType 2 haskellDiv

haskellDiv :: [Data] -> Data
haskellDiv (_:[DInt 0]) = DError "Trying to divide by 0"
haskellDiv ((DInt x):[DInt y]) = DInt (x `div` y)
haskellDiv (DError e:_) = DError e
haskellDiv (_:[DError e]) = DError e
haskellDiv _ = DError $ "Given argument of wrong type " ++
                     "to '/' operator. Expected type: Int"


primiMod :: PrimitiveFunc
primiMod = PrimitiveFunc "Modulo" arOpType 2 haskellMod

haskellMod :: [Data] -> Data
haskellMod (_:[DInt 0]) = DError "Trying to apply modulo operation to 0"
haskellMod ((DInt x):[DInt y]) = DInt (x `mod` y)
haskellMod (DError e:_) = DError e
haskellMod (_:[DError e]) = DError e
haskellMod _ = DError $ "Given argument of wrong type " ++
                     "to '%' operator. Expected type: Int"



----------------------------------------- PrimitiveFunc logical operatorations ---------------------------------

primiAnd :: PrimitiveFunc
primiAnd = PrimitiveFunc "And" logOpType 2 haskellAnd

haskellAnd :: [Data] -> Data
haskellAnd ((DBool b1):[DBool b2]) = DBool (b1 && b2)
haskellAnd (DError e:_) = DError e
haskellAnd (_:[DError e]) = DError e
haskellAnd _ = DError $ "Given argument of wrong type " ++
                     "to '&&' operator. Expected type: Bool"


primiOr :: PrimitiveFunc
primiOr = PrimitiveFunc "Or" logOpType 2 haskellOr

haskellOr :: [Data] -> Data
haskellOr ((DBool b1):[DBool b2]) = DBool (b1 || b2)
haskellOr (DError e:_) = DError e
haskellOr (_:[DError e]) = DError e
haskellOr _ = DError $ "Given argument of wrong type " ++
                     "to '||' operator. Expected type: Bool"



----------------------------------------- PrimitiveFunc comparison operations ---------------------------------

primiEq :: PrimitiveFunc
primiEq = PrimitiveFunc "Equal" comOpType 2 haskellEq

haskellEq :: [Data] -> Data
haskellEq ((DInt x):[DInt y]) = DBool (x == y)
haskellEq (DError e:_) = DError e
haskellEq (_:[DError e]) = DError e
haskellEq _ = DError $ "Given argument of wrong type " ++
                     "to '==' operator. Expected type: Int"


primiNotEq :: PrimitiveFunc
primiNotEq = PrimitiveFunc "Not equal" comOpType 2 haskellNotEq

haskellNotEq :: [Data] -> Data
haskellNotEq ((DInt x):[DInt y]) = DBool (not (x == y))
haskellNotEq (DError e:_) = DError e
haskellNotEq (_:[DError e]) = DError e
haskellNotEq _ = DError $ "Given argument of wrong type " ++
                     "to '!=' operator. Expected type: Int"


primiLess :: PrimitiveFunc
primiLess = PrimitiveFunc "Less" comOpType 2 haskellLess

haskellLess :: [Data] -> Data
haskellLess ((DInt x):[DInt y]) = DBool (x < y)
haskellLess (DError e:_) = DError e
haskellLess (_:[DError e]) = DError e
haskellLess _ = DError $ "Given argument of wrong type " ++
                     "to '<' operator. Expected type: Int"


primiMore :: PrimitiveFunc
primiMore = PrimitiveFunc "More" comOpType 2 haskellMore

haskellMore :: [Data] -> Data
haskellMore ((DInt x):[DInt y]) = DBool (x > y)
haskellMore (DError e:_) = DError e
haskellMore (_:[DError e]) = DError e
haskellMore _ = DError $ "Given argument of wrong type " ++
                     "to '>' operator. Expected type: Int"



------------------------------------------- If statement ----------------------------------------

primiIf :: PrimitiveFunc
primiIf = PrimitiveFunc "builtinIf" 
            (TypeFunc TypeBool $ TypeFunc (TypeVar "a") $ TypeFunc (TypeVar "a") (TypeVar "a")) 
            3 haskellIf

haskellIf :: [Data] -> Data 
haskellIf [DError e, _, _] = DError e
haskellIf [DBool b, e1, e2] = if b then e1 else e2
haskellIf _ = DError "Given not a boolean expression in if statement"




----------------------------------------- PrimitiveFuncs for list operations ---------------------------------

primiEmpty :: PrimitiveFunc
primiEmpty = PrimitiveFunc "Empty" (TypeList (TypeVar "a")) 0 haskellEmpty

haskellEmpty :: [Data] -> Data 
haskellEmpty _ = DEvaluatedList []



primiIsEmpty :: PrimitiveFunc
primiIsEmpty = PrimitiveFunc "Is empty" (TypeFunc (TypeList (TypeVar "a")) (TypeBool)) 
                                    1 haskellIsEmpty

haskellIsEmpty :: [Data] -> Data
haskellIsEmpty [DError err] = DError (err ++ ". Error inside list")
haskellIsEmpty [DList l] = DBool (null l)
haskellIsEmpty [DEvaluatedList l] = DBool (null l)
haskellIsEmpty _ = 
    DError $ "Trying to apply 'isEmpty' to not-list object"



primiHead :: PrimitiveFunc
primiHead = PrimitiveFunc "Head" (TypeFunc (TypeList (TypeVar "a")) ((TypeVar "a"))) 
                                1 haskellHead

haskellHead :: [Data] -> Data
haskellHead [DError err] = DError (err ++ ". Error inside list")
haskellHead [DList _] = DError $ "Trying to apply 'head' to not-evaluated list"
haskellHead [DEvaluatedList l] =
    case l of
        [] -> DError $ "Trying to apply 'head' to empty list"
        _ -> head l
haskellHead _ = 
    DError $ "Trying to apply 'head' to not-list object"



primiTail :: PrimitiveFunc
primiTail = PrimitiveFunc "Tail" (TypeFunc (TypeList (TypeVar "a")) (TypeList (TypeVar "a"))) 
                                1 haskellTail

haskellTail :: [Data] -> Data 
haskellTail [DError err] = DError (err ++ ". Error inside list")
haskellTail [DList _] = DError $ "Trying to apply 'tail' to not-evaluated list"
haskellTail [DEvaluatedList l] =
    case l of
        [] -> DError $ "Trying to apply 'tail' to empty list"
        _ -> DEvaluatedList (tail l)
haskellTail _ = DError $ "Trying to apply 'tail' to not-list object"



primiConcat :: PrimitiveFunc
primiConcat = PrimitiveFunc "Concat" 
        (TypeFunc (TypeList (TypeVar "a")) $ TypeFunc (TypeList (TypeVar "a")) $ TypeList (TypeVar "a")) 
        2 haskellConcat

haskellConcat :: [Data] -> Data 
haskellConcat (DError err:_) = DError (err ++ ". Error inside first list")
haskellConcat (_:[DError err]) = DError (err ++ ". Error inside second list")
haskellConcat (DList _:[DList _]) = DError $ "Trying to apply 'concat' to not-evaluated lists"
haskellConcat (DEvaluatedList l1:[DEvaluatedList l2]) = 
    DEvaluatedList (l1 ++ l2)
haskellConcat _ = DError $ "Trying to apply 'concat' to not-list object"


