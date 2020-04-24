{-# Options -Wall -Wname-shadowing #-}


module Executor where

import qualified Data.Map.Lazy as M
import Text.Megaparsec
--import Text.Megaparsec.Char

import Parser
import Definitions
import TreeEvaluator
import Printer


-- TODO
-- zmienic evaluateProgramm
-- zmienic getEnv
-- concat w executeProgramm



-- runProgramm parses input string. If the input is not
-- a proper programm returns parsing error. Otherwise it
-- returns programm's output
runProgramm :: String -> String -> IO ()
runProgramm file input = do
    let parsedProg = runParser parseProgramm file input
    case parsedProg of
        Left parse_error -> putStrLn (errorBundlePretty parse_error)
        Right programm -> putStrLn (executeProgramm programm)


-- executeProgramm executes given, parsed programm
-- and returns its string
executeProgramm :: Programm -> String
executeProgramm programm = let env = getEnv programm
                               results = evaluateProgramm env programm
                            in concat (map printProgResult results)


-- getEnv prepears and returns environment
-- for given programm
getEnv :: Programm -> Env
getEnv programm = 
    let 
        addToEnv :: ProgElem -> Env -> Env
        addToEnv (PEExpr _) env = env
        addToEnv (PEDef n t) env = M.insert n (evaluateTree t result) env
        addToEnv (PEFunc n t) env = M.insert n (evaluateTree t result) env
        result = foldr addToEnv M.empty programm
    in result


-- evaluateProgramm evaluates all expressions and function's
-- applications, which occur in given, parsed programm
evaluateProgramm :: Env -> Programm -> ProgResult
evaluateProgramm env programm = 
    let
        evalProgElem :: ProgElem -> ProgResult -> ProgResult
        evalProgElem (PEDef _ _) resList = resList
        evalProgElem (PEFunc _ _) resList = resList
        evalProgElem (PEExpr e) resList = evaluateTree e env : resList
    in foldr evalProgElem [] programm
{-
evalProgElem :: ProgElem -> ProgResult -> Env -> ProgResult
evalProgElem (PEDef _ _) resList env = resList
evalProgElem (PEExpr e) resList env = evaluateTree e env : resList
-}