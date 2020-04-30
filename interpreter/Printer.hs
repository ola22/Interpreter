{-# Options -Wall -Wname-shadowing #-}


module Printer where

import Definitions




-- Function used for printing programm's output
printProgResult :: Data -> String
printProgResult (DInt n) = show n ++ "\n"
printProgResult (DBool b) = show b ++ "\n"
printProgResult (DFunc name _ _) = "Given function of argument " ++ name ++ "\n"
printProgResult (DPrimi (PrimitiveFunc name _ _ _)) = "Primitive function: " ++ name ++ "\n"
printProgResult (DListPrimi (PrimitiveListFunc name _ _ _)) = "Primitive function: " ++ name ++ "\n"
printProgResult (DList l) = "Parse trees of list elements: [" ++ show l ++ "]\n"
printProgResult (DEvaluatedList l) = "List: [" ++ printList l ++ "]\n"
printProgResult (DError err) = "RUNTIME ERROR: " ++ err ++ "\n"


-- Function for pretty printing of lists
-- (with spaces and commas)
printList :: [Data] -> String
printList [] = ""
printList (h:[]) = show h
printList (h:t) = show h ++ ", " ++ printList t 
