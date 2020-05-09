{-# Options -Wall -Wname-shadowing #-}


module Executor where

import qualified Data.Map.Lazy as M
import System.IO (stderr, stdout , hPutStrLn)
import Text.Megaparsec
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Parser
import Definitions
import TreeEvaluator
import Printer
import TypeChecker


-- TODO
-- def i func w getEnv trzeba putStrLn na wyjście






-- runProgramm parses input string. If the input is not
-- a proper programm/non-typing program, it returns parsing/typecheck error.
-- Otherwise it returns programm's output.
runProgramm :: String -> String -> IO ()
runProgramm file input = do
    ololLib <- readFile "lib.olol"
    let parsedProg = runParser parseProgramm file (input ++ "\n" ++ ololLib)
    case parsedProg of
        Left parse_error -> hPutStrLn stderr (errorBundlePretty parse_error)
        Right programm -> 
            let res = evalState (runReaderT (runErrorT  (typeCheck programm)) TIEnv) 0
            in case res of
                Left err -> hPutStrLn stderr ("TYPECHECK ERROR: " ++ err)
                Right _ -> hPutStrLn stdout (executeProgramm programm)


-- executeProgramm first typechecks and than executes given, 
-- parsed programm and returns its string.
executeProgramm :: Programm -> String
executeProgramm programm = let env = getEnv programm
                               results = reverse (evaluateProgramm env programm [])
                            in concat (map printProgResult results)


-- getEnv prepears and returns environment
-- for given programm.
getEnv :: Programm -> Env
getEnv programm = 
    let 
        addDeclsToEnv :: ProgElem -> Env -> Env
        addDeclsToEnv (PEExpr _) env = env
        addDeclsToEnv (PEDef pos n t) env = 
            let lookRes = M.lookup n env
            in case lookRes of
                Just _ -> M.insert n (DError $ addPosToDeclError pos n "variable") env
                Nothing -> M.insert n (evaluateTree result t) env
        addDeclsToEnv (PEFunc pos n t) env = 
            let lookRes = M.lookup n env
            in case lookRes of
                Just _ -> M.insert n (DError $ addPosToDeclError pos n "function") env
                Nothing -> M.insert n (evaluateTree result t) env
        result = foldr addDeclsToEnv M.empty programm
    in result


-- evaluateProgramm evaluates all expressions and function's
-- applications, which occur in given, parsed programm.
-- Evaluation stops after getting error.
evaluateProgramm :: Env -> Programm -> ProgResult -> ProgResult
evaluateProgramm _ [] resList = resList
evaluateProgramm env ((PEDef _ _ _):rest) resList = evaluateProgramm env rest resList
evaluateProgramm env ((PEFunc _ _ _):rest) resList = evaluateProgramm env rest resList
evaluateProgramm env ((PEExpr e):rest) resList = 
    let evaluated = evaluateTree env e 
    in case evaluated of
        DError _ -> evaluated:resList
        _ -> evaluateProgramm env rest (evaluated:resList)

