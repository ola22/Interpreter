module Evaluation where

import Data.Maybe
import qualified Data.Map.Lazy as M
import Definitions

-- ZANIM NA GITA:
-- zmienic primitive
-- zmienic Show
-- zmienic testy
-- dopisac testy
-- data???
-- zmienic eval??-- co interpreter a co eval???????
-- tego evala jakos inaczej :o




-- Function interprets and executes given ParseTree.
eval :: ParseTree -> Env -> Data
eval (TData tdata) _ = tdata
eval (TVar var) env = fromJust $ M.lookup var env
eval (TFAppl f v) env = let func = eval f env -- :: Dfunc/DPrimi
                            var_val = eval v env -- :: DInt/DBool
                        in case func of
                                DPrimi p -> make_primi p var_val
                                DFunc var_name ftree fenv -> eval ftree (M.insert var_name var_val fenv)
                                _ -> undefined -- error "not proper function"


-- prymitywki :o <3
make_primi :: Primitive -> Data -> Data
make_primi (Primitive 1 func) var = func [var]
make_primi (Primitive n func) var = DPrimi (Primitive (n-1) (\x -> func (var:x)))   --- czy tego n9e zmienic na cos mniej trickowego???




----------------------------------------- Primitive arithmetical operations ---------------------------------


primi_add :: Primitive
primi_add = Primitive 2 haskell_add

haskell_add :: [Data] -> Data
haskell_add ((DInt x):[DInt y]) = DInt (x + y)
haskell_add _ = undefined -- error "wrong type"


primi_sub :: Primitive
primi_sub = Primitive 2 haskell_sub

haskell_sub :: [Data] -> Data
haskell_sub ((DInt x):[DInt y]) = DInt (x - y)
haskell_sub _ = undefined -- error "wrong type"


primi_mul :: Primitive
primi_mul = Primitive 2 haskell_mul

haskell_mul :: [Data] -> Data
haskell_mul ((DInt x):[DInt y]) = DInt (x * y)
haskell_mul _ = undefined -- error "wrong type"


primi_div :: Primitive
primi_div = Primitive 2 haskell_div

haskell_div :: [Data] -> Data
haskell_div ((DInt x):[DInt y]) = DInt (x `div` y)
haskell_div _ = undefined -- error "wrong type"


primi_mod :: Primitive
primi_mod = Primitive 2 haskell_mod

haskell_mod :: [Data] -> Data
haskell_mod ((DInt x):[DInt y]) = DInt (x `mod` y)
haskell_mod _ = undefined -- error "wrong type"



----------------------------------------- Primitive logical operatorations ---------------------------------

primi_and :: Primitive
primi_and = Primitive 2 haskell_and

haskell_and :: [Data] -> Data
haskell_and ((DBool b1):[DBool b2]) = DBool (b1 && b2)
haskell_and _ = undefined -- error "wrong type"


primi_or :: Primitive
primi_or = Primitive 2 haskell_or

haskell_or :: [Data] -> Data
haskell_or ((DBool b1):[DBool b2]) = DBool (b1 || b2)
haskell_or _ = undefined -- error "wrong type"


primi_not :: Primitive
primi_not = Primitive 1 haskell_not

haskell_not :: [Data] -> Data
haskell_not [DBool b] = DBool (not b)
haskell_not _ = undefined -- error "wrong type"



----------------------------------------- Primitive comparison operations ---------------------------------

primi_eq :: Primitive
primi_eq = Primitive 2 haskell_eq

haskell_eq :: [Data] -> Data
haskell_eq ((DInt x):[DInt y]) = DBool (x == y)
haskell_eq _ = undefined -- error "wrong type"


primi_not_eq :: Primitive
primi_not_eq = Primitive 2 haskell_not_eq

haskell_not_eq :: [Data] -> Data
haskell_not_eq ((DInt x):[DInt y]) = DBool (not (x == y))
haskell_not_eq _ = undefined -- error "wrong type"


primi_less :: Primitive
primi_less = Primitive 2 haskell_less

haskell_less :: [Data] -> Data
haskell_less ((DInt x):[DInt y]) = DBool (x < y)
haskell_less _ = undefined -- error "wrong type"


primi_more :: Primitive
primi_more = Primitive 2 haskell_more

haskell_more :: [Data] -> Data
haskell_more ((DInt x):[DInt y]) = DBool (x >y)
haskell_more _ = undefined -- error "wrong type"




{- | Basic eval tests
>>> eval ( TData $ DInt 5 ) M.empty
DInt 5
>>> eval ( TData $ DBool True ) M.empty
DBool True

>>> eval ( TData $ DInt 5 ) M.empty
DInt 5

>>> eval (TFAppl (TData (DFunc "x" ( TFAppl ( TFAppl (TData $ DPrimi primi_mul) (TVar "x") ) (TVar "x")) M.empty)) (TData $ DInt 5)) M.empty
DInt 25
-}


{- | Primitives
>>> eval ( TFAppl ( TFAppl (TData $DPrimi primi_add) (TData $ DInt 2) ) (TData $ DInt 2)) M.empty
DInt 4
>>> eval ( TFAppl ( TFAppl (TData $DPrimi primi_sub) (TData $ DInt 5) ) (TData $ DInt 2)) M.empty
DInt 3
>>> eval ( TFAppl ( TFAppl (TData $DPrimi primi_mul) (TData $ DInt 5) ) (TData $ DInt 2)) M.empty
DInt 10
>>> eval ( TFAppl ( TFAppl (TData $DPrimi primi_div) (TData $ DInt 10) ) (TData $ DInt 3)) M.empty
DInt 3
>>> eval ( TFAppl ( TFAppl (TData $DPrimi primi_mod) (TData $ DInt 10) ) (TData $ DInt 3)) M.empty
DInt 1
>>> eval ( TFAppl ( TFAppl (TData $DPrimi primi_and) (TData $ DBool True) ) (TData $ DBool True)) M.empty
DBool True
-}