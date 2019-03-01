module Evaluation where

import Reduction

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy prog term | isNormalForm prog term = []
                     | otherwise = []

liStrategy :: Strategy

roStrategy :: Strategy

riStrategy :: Strategy

poStrategy :: Strategy

piStrategy :: Strategy

reduceWith :: Strategy -> Prog -> Term -> Maybe Term

evaluateWith :: Strategy -> Prog -> Term -> Term
