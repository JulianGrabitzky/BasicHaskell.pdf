module Matching(match) where

import Term
import Substitution
import Data.Maybe

-- Create a substitution from the first term to the second term
match :: Term -> Term -> Maybe Subst
match (Var varName) term    = Just (single varName term)
match (Comb c list) (Var _) = Nothing
match (Comb combName1 list1) (Comb combName2 list2) | combName1    /= combName2    = Nothing
                                                    | length list1 /= length list2 = Nothing
                                                    | otherwise                    = foldl1 composeMaybeSubst (zipWith match list1 list2)

composeMaybeSubst :: Maybe Subst -> Maybe Subst -> Maybe Subst
composeMaybeSubst (Just s1) (Just s2) = Just (compose s1 s2)
composeMaybeSubst _         _         = Nothing

applyMaybe :: Maybe Subst -> Term -> Term
applyMaybe (Just s) t = apply s t
applyMaybe Nothing  _ = error "No match found."

t1 = Comb "add" [(Var "x"), (Var "y")]
t2 = Comb "add" [(Var "1"), (Var "2")]

s1 = single "x" (Var "1") -- \(Var "x") -> (Var "1")
s2 = single "y" (Var "2") -- \(Var "y") -> (Var "2")