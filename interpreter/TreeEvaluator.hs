{-# Options -Wall -Wname-shadowing #-}

module TreeEvaluator where

--import Data.Maybe
import qualified Data.Map.Lazy as M
import Definitions


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




-- Function interprets and executes given ParseTree.
evaluateTree :: ParseTree -> Env -> Data
evaluateTree (TData tdata) _ = tdata   --- ok
evaluateTree (TVar var) env =                        -- TU ZMIENIC ok
    let maybeVar = M.lookup var env
    in case maybeVar of
            Nothing -> DError ("Used invalid identifier: " ++ var)
            Just res -> res
evaluateTree (TFunc var_name tree) env = DFunc var_name tree env   ---- ok
evaluateTree (TFAppl f v) env = 
    let func = evaluateTree f env -- :: Dfunc/DPrimi
        var_val = evaluateTree v env -- :: DInt/DBool
    in case func of
            DPrimi p -> evaluatePrimi p var_val                        -- czy
            DFunc var_name ftree fenv ->
                case var_val of
                    DError err -> DError err
                    _ -> evaluateTree ftree (M.insert var_name var_val fenv)    -- czy tu nie moze byc jeszcze cos innego
            DError err -> DError err                                               -- czy to napewno tak???
            _ -> DError ("Trying to apply to sth, that is not a proper function")


-- prymitywki :o <3
evaluatePrimi :: PrimitiveFunc -> Data -> Data
evaluatePrimi (PrimitiveFunc 1 func) var = func [var]
evaluatePrimi (PrimitiveFunc n func) var = DPrimi (PrimitiveFunc (n-1) (\x -> func (var:x)))
evaluatePrimi _ (DError err) = DError err




----------------------------------------- PrimitiveFunc arithmetical operations ---------------------------------


primiAdd :: PrimitiveFunc
primiAdd = PrimitiveFunc 2 haskellAdd

haskellAdd :: [Data] -> Data
haskellAdd ((DInt x):[DInt y]) = DInt (x + y)
haskellAdd _ = undefined -- error "wrong type"


primiSub :: PrimitiveFunc
primiSub = PrimitiveFunc 2 haskellSub

haskellSub :: [Data] -> Data
haskellSub ((DInt x):[DInt y]) = DInt (x - y)
haskellSub _ = undefined -- error "wrong type"


primiMul :: PrimitiveFunc
primiMul = PrimitiveFunc 2 haskellMul

haskellMul :: [Data] -> Data
haskellMul ((DInt x):[DInt y]) = DInt (x * y)
haskellMul _ = undefined -- error "wrong type"


primiDiv :: PrimitiveFunc
primiDiv = PrimitiveFunc 2 haskellDiv

haskellDiv :: [Data] -> Data
haskellDiv ((DInt x):[DInt y]) = DInt (x `div` y)
haskellDiv _ = undefined -- error "wrong type"


primiMod :: PrimitiveFunc
primiMod = PrimitiveFunc 2 haskellMod

haskellMod :: [Data] -> Data
haskellMod ((DInt x):[DInt y]) = DInt (x `mod` y)
haskellMod _ = undefined -- error "wrong type"



----------------------------------------- PrimitiveFunc logical operatorations ---------------------------------

primiAnd :: PrimitiveFunc
primiAnd = PrimitiveFunc 2 haskellAnd

haskellAnd :: [Data] -> Data
haskellAnd ((DBool b1):[DBool b2]) = DBool (b1 && b2)
haskellAnd _ = undefined -- error "wrong type"


primiOr :: PrimitiveFunc
primiOr = PrimitiveFunc 2 haskellOr

haskellOr :: [Data] -> Data
haskellOr ((DBool b1):[DBool b2]) = DBool (b1 || b2)
haskellOr _ = undefined -- error "wrong type"


----------------------------------------- PrimitiveFunc comparison operations ---------------------------------

primiEq :: PrimitiveFunc
primiEq = PrimitiveFunc 2 haskellEq

haskellEq :: [Data] -> Data
haskellEq ((DInt x):[DInt y]) = DBool (x == y)
haskellEq ((DBool b1):[DBool b2]) = DBool (b1 == b2)
haskellEq _ = undefined -- error "wrong type"


primiNotEq :: PrimitiveFunc
primiNotEq = PrimitiveFunc 2 haskellNotEq

haskellNotEq :: [Data] -> Data
haskellNotEq ((DInt x):[DInt y]) = DBool (not (x == y))
haskellNotEq ((DBool b1):[DBool b2]) = DBool (not (b1 == b2))
haskellNotEq _ = undefined -- error "wrong type"


primiLess :: PrimitiveFunc
primiLess = PrimitiveFunc 2 haskellLess

haskellLess :: [Data] -> Data
haskellLess ((DInt x):[DInt y]) = DBool (x < y)
haskellLess _ = undefined -- error "wrong type"


primiMore :: PrimitiveFunc
primiMore = PrimitiveFunc 2 haskellMore

haskellMore :: [Data] -> Data
haskellMore ((DInt x):[DInt y]) = DBool (x > y)
haskellMore _ = undefined -- error "wrong type"


------------------------------------------- If statement ----------------------------------------

primiIf :: PrimitiveFunc
primiIf = PrimitiveFunc 3 haskellIf

haskellIf :: [Data] -> Data
haskellIf [DBool b, e1, e2] = if b then e1 
                                   else e2
haskellIf _ = undefined -- error "not a boolean expression in if statement"




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