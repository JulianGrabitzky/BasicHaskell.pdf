module Evaluation
    (Strategy, loStrategy, liStrategy, roStrategy, riStrategy, poStrategy,
     piStrategy)
    where

import Reduction
import Position
import Term
import Prog

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy = createStrategy leftOf above

liStrategy :: Strategy
liStrategy = createStrategy leftOf below

roStrategy :: Strategy

riStrategy :: Strategy

poStrategy :: Strategy

piStrategy :: Strategy

-- Vllt wird das ja was
createStrategy :: (Pos -> Pos -> Bool) ->
                  (Pos -> Pos -> Bool) ->
                  Strategy
createStrategy func1 func2 prog term
    | isNormalForm prog term = []
    | otherwise =
        -- vergleiche Positionen irgendwie mit functionen oder so
        foldl1 (\pos1 pos2 -> ) $ reduciblePos prog term

reduceWith :: Strategy -> Prog -> Term -> Maybe Term

evaluateWith :: Strategy -> Prog -> Term -> Term
