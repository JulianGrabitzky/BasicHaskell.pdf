{-# LANGUAGE TemplateHaskell #-}
module ArbitraryTerm where

import src/Term

instance Arbitrary Var where
    var = do
    a <- elements ['a'..'z']
    return Var a


return []
testAll = $quickCheck


t = (Comb "mul" [(Var "x"), (Comb "add" [(Var "y"), (Var "z"), (Var "w")])])

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
