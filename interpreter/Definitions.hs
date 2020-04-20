{-# Options -Wall -Wname-shadowing #-}

module Definitions where

import qualified Data.Map.Lazy as M

-- TODO
-- zmienic instance show data




-- Env is a map storeing variables and theirs values,
-- functions and theirs "bodies"
type Env = M.Map String Data


-- Data is a stucture for all possible programm and functions' outputs
-- as well as functions' bodies
data Data = DInt Integer 
            | DBool Bool 
            | DFunc String ParseTree Env  -- bo statyczne wiazanie identyfikatorow, jeden arg to name
            | DPrimi PrimitiveFunc

instance Show Data where
    show (DInt x) = "DInt " ++ show x
    show (DBool b) = "DBool " ++ show b
    show (DFunc s t e) = "DFun " ++ show s ++ show t ++ show e
    show (DPrimi (PrimitiveFunc n _)) = "DPrimi " ++ show n


-- ParseTree is a tree storeing parsed programm where:
--  * Tdata - leaf storeing simple data (eg. 5)
--  * TVar - leaf storeing variables, operators, func names (eg. "x", "+")
--  * TFAppl - node storeing two ParseTrees (first is a TVar: operator/func or TFAppl
--             and second is TVar: var or TData)
data ParseTree = TData Data 
                | TVar String 
                | TFAppl ParseTree ParseTree
                | TFunc String ParseTree
    deriving Show


-- Primitive is created for some primitive operations like arithmetical,
-- logical or comparison operations and other simple statements
data PrimitiveFunc = PrimitiveFunc Int ([Data] -> Data)


-- ProgElem are all possible programm elemrnts: expression,
-- variable definiction and function definition
data ProgElem = PEExpr ParseTree
                | PEDef String ParseTree
                | PEFunc String ParseTree
    deriving Show


-- Programm is type representing a set of all,
-- already parsed programm elements.
type Programm = [ProgElem]


-- ProgResult is type storeing all
-- evaluation outputs evaluated from 
-- given programm
type ProgResult = [Data]

