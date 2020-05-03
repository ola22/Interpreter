module Main where

import System.Environment
import Control.Monad

import Executor





-- Main program function. It gets input from file
-- or from stdin and than runs program.
main :: IO ()
main = do 
    args <- getArgs
    (file, input) <- (uncurry $ liftM2 (,)) $ 
        if null args
            then (return "stdin", getContents)
            else (return (head args), readFile (head args))
    runProgramm file input
    