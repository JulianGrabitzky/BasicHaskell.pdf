module Matching(match) where

import Term
import Substitution

-- Create a substitution from the first term to the second term
match :: Term -> Term -> Maybe Subst
match (Var varName) term    = Just (single varName term)
match (Comb _ _)    (Var _) = Nothing
match (Comb combName1 list1) (Comb combName2 list2) | combName1 == combName2 &&
                                                      length list1 == length list2 =
                                                      foldl composeMaybeSubst (Just identity) (zipWith match list1 list2)
                                                    | otherwise = Nothing

-- Compose two maybe subst
composeMaybeSubst :: Maybe Subst -> Maybe Subst -> Maybe Subst
composeMaybeSubst (Just s1) (Just s2) = Just (compose s1 s2)
composeMaybeSubst _         _         = Nothing
