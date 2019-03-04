module Evaluation
    (Strategy, loStrategy, liStrategy)--, roStrategy, riStrategy, poStrategy,piStrategy)
    where

import Reduction
import Position
import Term
import Prog

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy = createStrategy above leftOf

liStrategy :: Strategy
liStrategy = createStrategy below leftOf

roStrategy :: Strategy
roStrategy = createStrategy above rightOf

riStrategy :: Strategy
riStrategy = createStrategy below rightOf

-- poStrategy :: Strategy

-- piStrategy :: Strategy

createStrategy :: (Pos -> Pos -> Bool) ->
                  (Pos -> Pos -> Bool) ->
                  Strategy
createStrategy ab lr prog term | isNormalForm prog term = []
                               | otherwise =
                                 [helper (head (reduciblePos prog term)) (reduciblePos prog term)]
    where
      helper :: Pos -> [Pos] -> Pos
      helper p1 [] = p1
      helper p1 (p2:ps) | (ab p1 p2) || (lr p1 p2) = helper p1 ps
                        | otherwise                = helper p2 ps


program = (Prog [r1, r2])
r1 = (Rule left1 right1)
r2 = (Rule left2 right2)

left1 = (Var "x")
right1 = (Var "y")

left2 = (Var "y")
right2 = (Var "z")

t1 = Comb "add" [(Var "x"), (Var "y")]
t2 = Comb "add" [(Var "1"), (Var "2")]
-- reduceWith :: Strategy -> Prog -> Term -> Maybe Term

-- evaluateWith :: Strategy -> Prog -> Term -> Term

{-
-- Vllt wird das ja was
createStrategy :: (Pos -> Pos -> Bool) -> (Pos -> Pos -> Bool) -> Strategy
createStrategy func1 func2 prog term
| isNormalForm prog term = []
| otherwise =
  -- vergleiche Positionen irgendwie mit functionen oder so
  foldl1 (\pos1 pos2 -> if func1 pos1 pos2 || func2 pos1 pos2
    then p1
    else p2) $ reduciblePos prog term
-}
