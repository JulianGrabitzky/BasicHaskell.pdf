{-# LANGUAGE TemplateHaskell #-}
module TestPrettyPrinting where

import PrettyPrinting
import Term
import Test.QuickCheck

prop_Var :: String -> Bool
prop_Var s = pretty (Var s) == s

prop_Comb :: String -> String -> Bool
prop_Comb s1 s2 = pretty (Comb s1 [Comb s2 []]) == s1 ++ " " ++ s2

prop_CombWithList :: Bool
prop_CombWithList = pretty (Comb "mul" [(Var "x"), (Comb "add" [(Var "y"), (Var "z"), (Var "w")])])
                    == "mul x (add y z w)"

return []

testPretty :: IO Bool
testPretty = $quickCheckAll
