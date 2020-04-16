module Definitions where

import qualified Data.Map.Lazy as M




-- Env is a map storeing variables and theirs values,
-- functions and theirs "bodies"
type Env = M.Map String Data


-- Data is a stucture for all possible programm and functions' outputs
-- as well as functions' bodies
data Data = DInt Integer 
            | DBool Bool 
            | DFunc String ParseTree Env  -- bo statyczne wiazanie identyfikatorow, jeden arg to name
            | DPrimi Primitive

instance Show Data where                                                                --- to zmienic
    show (DInt x) = "DInt " ++ show x
    show (DBool b) = "DBool " ++ show b
    show (DFunc s t e) = "DFun " ++ show s ++ show t ++ show e
    show (DPrimi (Primitive n f)) = "DPrimi " ++ show n


-- ParseTree is a tree storeing parsed programm where:
--  * Tdata - leaf storeing simple data (eg. 5)
--  * TVar - leaf storeing variables, operators, func names (eg. "x", "+")
--  * TFAppl - node storeing two ParseTrees (first is a TVar: operator/func or TFAppl
--             and second is TVar: var or TData)
data ParseTree = TData Data 
                | TVar String 
                | TFAppl ParseTree ParseTree
                | TLambda String ParseTree
                | ADef [(String, ParseTree)] ParseTree
    deriving Show


-- Primitive is created for some primitive operations like arithmetical,
-- logical or comparison operations and other simple statements
data Primitive = Primitive Int ([Data] -> Data)
