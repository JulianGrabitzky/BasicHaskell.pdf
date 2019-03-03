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

liStrategy :: Strategy

roStrategy :: Strategy

riStrategy :: Strategy

poStrategy :: Strategy

piStrategy :: Strategy

reduceWith :: Strategy -> Prog -> Term -> Maybe Term

evaluateWith :: Strategy -> Prog -> Term -> Term
