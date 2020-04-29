{-# Options -Wall -Wname-shadowing #-}

module Definitions where

import qualified Data.Map.Lazy as M




-- Env is a map storeing variables and theirs values,
-- functions and theirs "bodies"
type Env = M.Map String Data


-- Data is a stucture for all possible programm and functions' outputs
-- as well as functions' bodies
data Data = DInt Integer 
            | DBool Bool 
            | DFunc String ParseTree Env  -- storeing env for static binding
            | DPrimi PrimitiveFunc
            | DListPrimi PrimitiveListFunc
            | DError String
            | DList [ParseTree]
            | DEvaluatedList [Data]

instance Show Data where
    show (DInt x) = show x 
    show (DBool b) = show b
    show (DFunc s _ _) = "Given function of argument " ++ show s
    show (DPrimi (PrimitiveFunc n _)) = "DPrimi " ++ show n
    show (DListPrimi (PrimitiveListFunc n _)) = "DListPrimi " ++ show n
    show (DError err) = "DError: " ++ err
    show (DList l) = "DList " ++ show l
    show (DEvaluatedList l) = "List : " ++ show l


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

data PrimitiveListFunc = PrimitiveListFunc Int (Env -> [Data] -> Data)


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

