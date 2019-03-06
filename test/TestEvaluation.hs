{-# LANGUAGE TemplateHaskell #-}
module TestEvaluation where

import Evaluation
import Term
import Prog

import Test.QuickCheck

prop_liStrategy :: Bool
prop_liStrategy = liStrategy program t1 == [[1]]

prop_loStrategy :: Bool
prop_loStrategy = loStrategy program t1 == [[]]

prop_riStrategy :: Bool
prop_riStrategy = riStrategy program t1 == [[2]]

prop_roStrategy :: Bool
prop_roStrategy = roStrategy program t1 == [[]]

program :: Prog
program = (Prog [r1, r2])

r1 :: Rule
r1 = (Rule left1 right1)
r2 :: Rule
r2 = (Rule left2 right2)

left1 :: Term
left1 = (Var "x")
right1 :: Term
right1 = (Var "y")

left2 :: Term
left2 = (Var "y")
right2 :: Term
right2 = (Var "z")

t1 :: Term
t1 = Comb "add" [(Var "x"), (Var "y")]
t2 :: Term
t2 = Comb "add" [(Comb "mul" [(Var "a"), (Var "b")]), (Comb "div" [(Var "c"), (Var "d")])]

return []
testEvaluation :: IO Bool
testEvaluation = $quickCheckAll
