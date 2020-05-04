{-# Options -Wall -Wname-shadowing #-}

module Definitions where

import qualified Data.Map.Lazy as M





-- Env is a map storeing variables and theirs values,
-- functions and theirs "bodies"
type Env = M.Map String Data


-- Type is a stucture for storeing all possible datatypes.
-- It is used in types inference algorithm.
data Type = TypeVar String
            | TypeInt
            | TypeBool
            | TypeList Type
            | TypeFunc Type Type
    deriving (Eq, Ord, Show)


-- Data is a stucture for all possible programm and functions' outputs
-- as well as functions' bodies
data Data = DInt Integer  -- store integer value
            | DBool Bool -- stores boolean value
            | DFunc String ParseTree Env  -- stores functions created by user (env for static binding)
            | DPrimi PrimitiveFunc  -- stores primitive (builtin) functions and operations
            | DListPrimi PrimitiveListFunc  -- stores builtin functions for lists
            | DError String  -- stores error messages
            | DList [ParseTree]  -- stores list with parse trees
            | DEvaluatedList [Data] -- stores list with already evaluated exprs

instance Show Data where
    show (DInt x) = show x 
    show (DBool b) = show b
    show (DFunc s _ _) = "Given function of argument " ++ show s
    show (DPrimi (PrimitiveFunc name _ n _)) = "DPrimi " ++ name ++ " " ++ show n
    show (DListPrimi (PrimitiveListFunc name _ n _)) = "DListPrimi " ++ name ++ " " ++ show n
    show (DError err) = "DError: " ++ err
    show (DList l) = "DList " ++ show l
    show (DEvaluatedList l) = "List : " ++ show l


-- FilePosition stores position in input file.
-- It stores file name and line number. It is
-- stored in ParseTree (each node stores its 
-- line number). Positions are used for writing errors.
data FilePosition = FilePosition String Integer deriving Show


-- ParseTree is a tree storeing parsed programm where:
--  * Tdata - leaf storeing simple data (eg. 5)
--  * TVar - leaf storeing variables, operators, func names (eg. "x", "+")
--  * TFAppl - node storeing two ParseTrees (first is a TVar: operator/func or TFAppl
--             and second is TVar: var or TData)
data ParseTree = TData FilePosition Data 
                | TVar FilePosition String 
                | TFAppl FilePosition ParseTree ParseTree
                | TFunc FilePosition String ParseTree
    deriving Show

-- Function returns position of given parse tree node
getPos :: ParseTree -> FilePosition
getPos (TData pos _) = pos
getPos (TVar pos _) = pos
getPos (TFAppl pos _ _) = pos
getPos (TFunc pos _ _) = pos


-- Primitive is created for some primitive operations like arithmetical,
-- logical or comparison operations and other simple statements.
-- Type is stored for type inference.
data PrimitiveFunc = PrimitiveFunc String Type Int ([Data] -> Data)

data PrimitiveListFunc = PrimitiveListFunc String Type Int (Env -> [Data] -> Data)


-- ProgElem are all possible programm elemrnts: expression,
-- variable definiction and function definition
data ProgElem = PEExpr ParseTree
                | PEDef FilePosition String ParseTree
                | PEFunc FilePosition String ParseTree
    deriving Show


-- Programm is type representing a set of all,
-- already parsed programm elements.
type Programm = [ProgElem]


-- ProgResult is type storeing all
-- evaluation outputs evaluated from 
-- given programm
type ProgResult = [Data]

