module Main where

import System.Environment
import Control.Monad

import Executor



main :: IO ()
main = do 
    args <- getArgs
    (file, input) <- (uncurry $ liftM2 (,)) $ 
        if null args
            then (return "stdin", getContents)
            else (return (head args), readFile (head args))
    -- putStrLn input
    -- putStrLn file
    runProgramm file input