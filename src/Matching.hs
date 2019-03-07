module Matching (match) where

import Substitution
import Term

-- Create a substitution from the first term to the second term.
match :: Term -> Term -> Maybe Subst
match (Var varName) term         = Just (single varName term)
match (Comb _ _)    (Var _)      = Nothing
match (Comb c1 l1)  (Comb c2 l2)
  | c1 == c2 && length l1 == length l2
  = foldl composeMaybeSubst (Just identity) (zipWith match l1 l2)
  | otherwise
  = Nothing

-- Compose two maybe subst to one.
composeMaybeSubst :: Maybe Subst -> Maybe Subst -> Maybe Subst
composeMaybeSubst (Just s1) (Just s2) = Just (compose s1 s2)
composeMaybeSubst _         _         = Nothing
