{-# Options -Wall -Wname-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeChecker where


import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe


import Definitions



















{-
    przechodze po adt i sprawdxam, czy typ jest dobry 
    mam data i jest int i jest bool
    dla prymitywow zapisuje sb typ, zeby potem sprawdzac 
    (==) :: Eq a => a -> a -> Bool -> niec hecmy == dla booli
    DFunc -> nie, bo w orginalnym drzewie parsowania nie powstanie funckja

    tak sie zachowuje na data

    TVar -> w tej funkcji trzymam śrdowisko mapa: string : typ 70 linia
    dla data wiemy
    dla var -> pilnujemy, żeby była anotacja typów
    
    aplikacja funkcji, czy typ argumentu sie zgadza z typem (cialo f, arg)
    no to patrzymy, jaki typ argumentu -> wywoluje f ktora pisze rekurencyjnie
    i patrze, czy to sie zgadza z tym, co mam w ciele funkcji)

    jak mam cialo funkcji, to spr czy funkcja, czy prymityw, no to dPrimi wiem, jakiego jest typu
    a dla dFunc trzeba cos podobnego do evala i jak wchodze do funkcji to wstawiam do srodowiska 
    i sprawdzam, czy sie robia

    ti :: TypeEnv -> ParseTree -> TypeM (Subst, Type) : subs to podstawienia

    zwracam typ wyrażeń
    A variable
        nie ma -> false
        jest -> true

    Aplikacja
        patrze czy typ lewego zgadza się z typem prawego

    scheme -> srodowisko typow jest ze stringow w scheme(zmienen wolne)
-}






