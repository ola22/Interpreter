{-# Options -Wall -Wname-shadowing #-}


module Executor where

import qualified Data.Map.Lazy as M
import Text.Megaparsec
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Parser
import Definitions
import TreeEvaluator
import Printer
import TypeChecker


-- TODO
-- def i func w getEnv trzeba putStrLn na wyjście



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
executeProgramm programm = 
        let res = evalState (runReaderT (runExceptT  (typeCheck programm)) TIEnv) 0
        in case res of
            Left err -> fail $ "TYPECHECK ERROR: " ++ err
            Right _ -> let env = getEnv programm
                           results = reverse (evaluateProgramm env programm [])
                        in concat (map printProgResult results)

{-
executeProgramm :: Programm -> String
executeProgramm programm = let env = getEnv programm
                               results = reverse (evaluateProgramm env programm [])
                            in concat (map printProgResult results)
-}


-- getEnv prepears and returns environment
-- for given programm.
getEnv :: Programm -> Env
getEnv programm = 
    let 
        addToEnv :: ProgElem -> Env -> Env
        addToEnv (PEExpr _) env = env
        addToEnv (PEDef n t) env = 
            let lookRes = M.lookup n env
            in case lookRes of
                Just _ -> M.insert n (DError ("Error during evaluation." ++ 
                                "Multiple declaration of variable: " ++ n)) env
                Nothing -> M.insert n (evaluateTree result t) env
        addToEnv (PEFunc n t) env = 
            let lookRes = M.lookup n env
            in case lookRes of
                Just _ -> M.insert n (DError ("Error during evaluation." ++ 
                                "Multiple declaration of function: " ++ n)) env
                Nothing -> M.insert n (evaluateTree result t) env
        result = foldr addToEnv M.empty programm
    in result


-- evaluateProgramm evaluates all expressions and function's
-- applications, which occur in given, parsed programm.
-- Evaluation stops after getting error.
evaluateProgramm :: Env -> Programm -> ProgResult -> ProgResult
evaluateProgramm _ [] resList = resList
evaluateProgramm env ((PEDef _ _):rest) resList = evaluateProgramm env rest resList
evaluateProgramm env ((PEFunc _ _):rest) resList = evaluateProgramm env rest resList
evaluateProgramm env ((PEExpr e):rest) resList = 
    let evaluated = evaluateTree env e 
    in case evaluated of
        DError _ -> evaluated:resList
        _ -> evaluateProgramm env rest (evaluated:resList)

