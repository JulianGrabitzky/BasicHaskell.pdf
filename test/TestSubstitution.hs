{-# LANGUAGE TemplateHaskell #-}
module TestSubstitution where

import Substitution
import Term
import Test.QuickCheck

prop_identity :: Bool
prop_identity = apply identity (Var "x") == Var "x"

prop_subWithSameVartoVar :: Bool
prop_subWithSameVartoVar = apply (single "x" (Var "x")) (Var "x") == Var "x"

prop_subWith2DiffVar :: Bool
prop_subWith2DiffVar = apply (single "x" (Var "x")) (Var "y") == Var "y"

prop_applySubToCombWithList :: Bool
prop_applySubToCombWithList = apply (single "x" (Var "69")) (Comb "1" [Var "x", Var "y"])
                              == Comb "1" (map (apply (single "x" (Var "69"))) [Var "x", Var "y"])
prop_composeIdFunction :: Bool
prop_composeIdFunction = apply (compose identity (single "x" (Var "69"))) (Var "x")
                         == apply (compose (single "x" (Var "69")) identity) (Var "x")

prop_composeWith2Subs :: Bool
prop_composeWith2Subs = apply (compose (single "x" (Var "1")) (single "y" (Var "2"))) (Comb "add" [Var "x", Var "y"])
                        == apply (single "y" (Var "2")) (apply (single "x" (Var "1")) (Comb "add" [Var "x", Var "y"]))

return []
testSubstitution :: IO Bool
testSubstitution = $quickCheckAll
