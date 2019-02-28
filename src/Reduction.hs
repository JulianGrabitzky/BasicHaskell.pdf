module Reduction where

import Term
import Position
import Matching
import Substitution
import Prog

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])              term = Nothing
findRule (Prog ((Rule left right):rs)) term | left == term = Just (right, substitution)
                                            | otherwise    = findRule (Prog rs) term
    where substitution = allOrNothing (match term right)
          allOrNothing :: Maybe Subst -> Subst
          allOrNothing Nothing      = error "No reduction ruls found."
          allOrNothing (Just subst) = subst

reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt (Prog []) _    _   = Nothing
reduceAt prog      term pos = case findRule prog (selectAt term pos) of
    Nothing -> Nothing
    Just (rhs, subst) -> Just (replaceAt term pos (apply subst rhs))

-- reduciblePos :: Prog -> Term -> [Pos]
-- reduciblePos

-- isNormalForm :: Prog -> Term -> Bool
-- isNormalForm

program = (Prog [r1, r2])

r1 = (Rule left1 right1)
r2 = (Rule left2 right2)

left1 = (Var "x")
right1 = (Var "1")

left2 = (Var "y")
right2 = (Comb "add" [(Var "a"), (Var "b")])

t = (Comb "mul" [(Var "x"), (Comb "add" [(Var "y"), (Var "z"), (Var "w")])])

gimmeDemRight :: Maybe (Rhs, Subst) -> Term
gimmeDemRight Nothing = error "No right term."
gimmeDemRight (Just (rhs, subst)) = rhs
