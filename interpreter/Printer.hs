{-# Options -Wall -Wname-shadowing #-}


module Printer where

import Definitions


-- TODO
-- zmienic nazwe funkcji na jakies stringify



-- Function used for printing programm's output
printProgResult :: Data -> String
printProgResult (DInt n) = show n ++ "\n"
printProgResult (DBool b) = show b ++ "\n"
printProgResult (DFunc name _ _) = "Given function od argument " ++ name ++ "\n"
printProgResult (DPrimi (PrimitiveFunc n _)) = show n ++ "\n"