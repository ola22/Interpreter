{-# Options -Wall -Wname-shadowing #-}


module Printer where

import Definitions




-- Function used for printing programm's output
printProgResult :: Data -> String
printProgResult (DInt n) = show n ++ "\n"
printProgResult (DBool b) = show b ++ "\n"
printProgResult (DFunc name _ _) = "Given function of argument " ++ name ++ "\n"
printProgResult (DPrimi (PrimitiveFunc n _)) = "Primitive with arg num: " ++ show n ++ "\n"
printProgResult (DListPrimi (PrimitiveListFunc n _)) = "Primitive with arg num: " ++ show n ++ "\n"
printProgResult (DList l) = "Parse trees of list elements: [" ++ show l ++ "]\n"
printProgResult (DEvaluatedList l) = "List: [" ++ printList l ++ "]\n"
printProgResult (DError err) = "ERROR: " ++ err ++ "\n"


-- Function for pretty printing of lists
-- (with spaces and commas)
printList :: [Data] -> String
printList [] = ""
printList (h:[]) = show h
printList (h:t) = show h ++ ", " ++ printList t 
